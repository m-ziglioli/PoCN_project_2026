#' Simulate SEIR Agent-Based Model on a Static Network
#' 
#' @param network data.frame with columns: node_from, node_to, weight
#' @param max_time maximum number of time steps (dt) to simulate
#' @param beta transmission rate per unit of contact weight
#' @param sigma probability of transitioning from Exposed to Infectious per time step
#' @param gamma probability of transitioning from Infectious to Recovered per time step
#' @param init_I_frac initial fraction of randomly selected infected individuals
#' @return data.frame with counts of S, E, I, R per time step

library(dplyr)

simulate_seir_static <- function(network, max_time = 7, beta = 0.01, sigma = 1/3, gamma = 1/5, init_I_frac = 0.05) {
  # Ensure necessary columns exist
  colnames(network) <- c("node_from", "node_to", "weight")

  
  # Ensure network is undirected for contacts (if A contacts B, B contacts A)
  net_reverse <- network
  net_reverse$node_from <- network$node_to
  net_reverse$node_to <- network$node_from
  net_full <- rbind(network, net_reverse)
  
  static_net <- net_full
  
  # Identify all unique nodes
  nodes <- as.character(unique(c(static_net$node_from, static_net$node_to)))
  N <- length(nodes)
  
  # Initialize states: S=1, E=2, I=3, R=4
  states <- rep(1, N) # All Susceptible
  names(states) <- nodes
  
  # Seed initial infections
  num_init_I <- max(1, round(N * init_I_frac))
  init_I_nodes <- sample(nodes, num_init_I)
  states[init_I_nodes] <- 3 # Set to Infectious
  
  # Results dataframe
  results <- data.frame(time = integer(), S = integer(), E = integer(), 
                        I = integer(), R = integer())
  
  # Simulation loop
  for (t in 1:max_time) {
    # Record state before transitions
    results <- rbind(results, data.frame(
      time = t,
      S = sum(states == 1),
      E = sum(states == 2),
      I = sum(states == 3),
      R = sum(states == 4)
    ))
    
    # Identify current infectious nodes
    infectious_nodes <- names(states)[states == 3]
    
    # Process infections if there are infectious nodes
    if (length(infectious_nodes) > 0) {
      
      # Filter for contacts where source is infectious and target is susceptible
      risk_contacts <- static_net %>%
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
    # Only consider nodes that were already infectious at the START of the time step
    if (length(infectious_nodes) > 0) {
      new_R <- infectious_nodes[runif(length(infectious_nodes)) < gamma]
      states[new_R] <- 4 # Transition I -> R
    }
    
    # Optional early stopping if no one is exposed or infectious
    if (sum(states %in% c(2, 3)) == 0) {
      # Record final state and break
      results <- rbind(results, data.frame(
        time = t + 1,
        S = sum(states == 1),
        E = sum(states == 2),
        I = sum(states == 3),
        R = sum(states == 4)
      ))
      break
    }
  }
  
  return(results)
}
