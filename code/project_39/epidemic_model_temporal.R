#' Simulate SEIR Agent-Based Model on a Temporal Network
#' 
#' @param network data.frame with columns: node_from, node_to, day, duration
#' @param beta transmission rate per unit of contact duration
#' @param sigma probability of transitioning from Exposed to Infectious per day
#' @param gamma probability of transitioning from Infectious to Recovered per day
#' @param init_I_frac initial fraction of randomly selected infected individuals
#' @return data.frame with daily counts of S, E, I, R

library(dplyr)

simulate_seir_temporal <- function(network, beta = 0.01, tau_E = 3, tau_I = 5, tau_R = Inf, init_I_frac = 0.05, scale_factor=15) {
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
  
  # Keep track of state entry times
  t_creation <- rep(0, N)
  names(t_creation) <- nodes
  
  # Seed initial infections
  num_init_I <- max(1, round(N * init_I_frac))
  init_I_nodes <- sample(nodes, num_init_I)
  states[init_I_nodes] <- 3 # Set to Infectious
  t_creation[init_I_nodes] <- 0
  
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
          mutate(prob_inf = 1 - exp(-beta * (total_weight/scale_factor)))
        
        # Sample new infections
        new_infections <- as.character(foi$node_to[runif(nrow(foi)) < foi$prob_inf])
        if (length(new_infections) > 0) {
          states[new_infections] <- 2 # Transition S -> E
          t_creation[new_infections] <- d
        }
      }
    }
    
    # Process Exposed to Infectious
    exposed_nodes <- names(states)[states == 2]
    if (length(exposed_nodes) > 0) {
      epsilon_E <- (d - t_creation[exposed_nodes]) / tau_E
      new_I <- exposed_nodes[runif(length(exposed_nodes)) < epsilon_E]
      if (length(new_I) > 0) {
        states[new_I] <- 3 # Transition E -> I
        t_creation[new_I] <- d
      }
    }
    
    # Process Infectious to Recovered
    # Only consider nodes that were already infectious at the START of the day
    if (length(infectious_nodes) > 0) {
      epsilon_I <- (d - t_creation[infectious_nodes]) / tau_I
      new_R <- infectious_nodes[runif(length(infectious_nodes)) < epsilon_I]
      if (length(new_R) > 0) {
        states[new_R] <- 4 # Transition I -> R
        t_creation[new_R] <- d
      }
    }

    # Process Recovered to Susceptible
    if (is.finite(tau_R)) {
      recovered_nodes <- names(states)[states == 4]
      if (length(recovered_nodes) > 0) {
        epsilon_R <- (d - t_creation[recovered_nodes]) / tau_R
        new_S <- recovered_nodes[runif(length(recovered_nodes)) < epsilon_R]
        if (length(new_S) > 0) {
          states[new_S] <- 1 # Transition R -> S
          t_creation[new_S] <- d
        }
      }
    }
  }
  
  return(results)
}