#' Simulate SEIR Agent-Based Model on a Temporal Network
#' 
#' @param network data.frame with columns: node_from, node_to, day, duration
#' @param beta transmission rate per unit of contact duration
#' @param sigma probability of transitioning from Exposed to Infectious per day
#' @param gamma probability of transitioning from Infectious to Recovered per day
#' @param init_I_frac initial fraction of randomly selected infected individuals
#' @return data.frame with daily counts of S, E, I, R

library(dplyr)

simulate_seir_temporal <- function(network, beta = 0.01, sigma = 1/3, gamma = 1/5, init_I_frac = 0.05) {
  # Ensure necessary columns exist
  colnames(network) <- c("day","node_from", "node_to", "duration", "contact_types")
  if (!all(c("node_from", "node_to", "day", "duration") %in% colnames(network))) {
    stop("Network must contain: node_from, node_to, day, duration")
  }
  
  # Ensure network is undirected for contacts (if A contacts B, B contacts A)
  net_reverse <- network
  net_reverse$node_from <- network$node_to
  net_reverse$node_to <- network$node_from
  net_full <- rbind(network, net_reverse)
  
  # Aggregate weights daily for each pair
  daily_net <- net_full %>%
    group_by(day, node_from, node_to) %>%
    summarise(weight = sum(duration), .groups = 'drop')
  
  # Identify all unique nodes
  nodes <- as.character(unique(c(daily_net$node_from, daily_net$node_to)))
  N <- length(nodes)
  
  # Initialize states: S=1, E=2, I=3, R=4
  states <- rep(1, N) # All Susceptible
  names(states) <- nodes
  
  # Seed initial infections
  num_init_I <- max(1, round(N * init_I_frac))
  init_I_nodes <- sample(nodes, num_init_I)
  states[init_I_nodes] <- 3 # Set to Infectious
  
  # Time range
  days <- sort(unique(daily_net$day))
  max_day <- max(days)
  
  # Results dataframe
  results <- data.frame(day = integer(), S = integer(), E = integer(), 
                        I = integer(), R = integer())
  
  # Simulation loop
  for (d in 1:max_day) {
    # Record daily state before transitions
    results <- rbind(results, data.frame(
      day = d,
      S = sum(states == 1),
      E = sum(states == 2),
      I = sum(states == 3),
      R = sum(states == 4)
    ))
    
    # Identify current infectious nodes
    infectious_nodes <- names(states)[states == 3]
    
    # Process infections if there are infectious nodes and contacts today
    if (length(infectious_nodes) > 0 && d %in% days) {
      contacts_today <- daily_net %>% filter(day == d)
      
      # Filter for contacts where source is infectious and target is susceptible
      risk_contacts <- contacts_today %>%
        filter(node_from %in% infectious_nodes, 
               states[as.character(node_to)] == 1)
      
      if (nrow(risk_contacts) > 0) {
        # Calculate force of infection on each susceptible node
        foi <- risk_contacts %>%
          group_by(node_to) %>%
          summarise(total_weight = sum(weight), .groups = 'drop') %>%
          mutate(prob_inf = 1 - exp(-beta * total_weight))
        
        # Sample new infections
        new_infections <- foi$node_to[runif(nrow(foi)) < foi$prob_inf]
        states[as.character(new_infections)] <- 2 # Transition S -> E
      }
    }
    
    # Process Exposed to Infectious
    exposed_nodes <- names(states)[states == 2]
    if (length(exposed_nodes) > 0) {
      new_I <- exposed_nodes[runif(length(exposed_nodes)) < sigma]
      states[new_I] <- 3 # Transition E -> I
    }
    
    # Process Infectious to Recovered
    # Only consider nodes that were already infectious at the START of the day
    if (length(infectious_nodes) > 0) {
      new_R <- infectious_nodes[runif(length(infectious_nodes)) < gamma]
      states[new_R] <- 4 # Transition I -> R
    }
  }
  
  return(results)
}