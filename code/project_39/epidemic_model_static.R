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

simulate_seir_static <- function(network, max_time = 3, beta = 0.01, tau_E = 5, tau_I = 14, tau_R = Inf, init_I_frac = 0.01, scale_factor=15) {
  # time is in days
  # tau_E: time needed to become I
  # tau_I: time needed to become R
  
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
  
  # Keep track of state entry times
  t_creation <- rep(0, N)
  names(t_creation) <- nodes
  
  # Seed initial infections
  num_init_I <- max(1, round(N * init_I_frac))
  init_I_nodes <- sample(nodes, num_init_I)
  states[init_I_nodes] <- 3 # Set to Infectious
  t_creation[init_I_nodes] <- 0
  
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
          mutate(prob_inf = 1 - exp(-beta * (total_weight/scale_factor)))
        
        # Sample new infections
        new_infections <- as.character(foi$node_to[runif(nrow(foi)) < foi$prob_inf])
        if (length(new_infections) > 0) {
          states[new_infections] <- 2 # Transition S -> E
          t_creation[new_infections] <- t
        }
      }
    }
    
    # Process Exposed to Infectious
    exposed_nodes <- names(states)[states == 2]
    if (length(exposed_nodes) > 0) {
      epsilon_E <- (t - t_creation[exposed_nodes]) / tau_E
      new_I <- exposed_nodes[runif(length(exposed_nodes)) < epsilon_E]
      if (length(new_I) > 0) {
        states[new_I] <- 3 # Transition E -> I
        t_creation[new_I] <- t
      }
    }
    
    # Process Infectious to Recovered
    # Only consider nodes that were already infectious at the START of the time step
    if (length(infectious_nodes) > 0) {
      epsilon_I <- (t - t_creation[infectious_nodes]) / tau_I
      new_R <- infectious_nodes[runif(length(infectious_nodes)) < epsilon_I]
      if (length(new_R) > 0) {
        states[new_R] <- 4 # Transition I -> R
        t_creation[new_R] <- t
      }
    }
    
    # Process Recovered to Susceptible
    if (is.finite(tau_R)) {
      recovered_nodes <- names(states)[states == 4]
      if (length(recovered_nodes) > 0) {
        epsilon_R <- (t - t_creation[recovered_nodes]) / tau_R
        new_S <- recovered_nodes[runif(length(recovered_nodes)) < epsilon_R]
        if (length(new_S) > 0) {
          states[new_S] <- 1 # Transition R -> S
          t_creation[new_S] <- t
        }
      }
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
