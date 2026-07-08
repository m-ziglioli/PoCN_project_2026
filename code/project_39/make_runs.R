library(parallel)

simulate_for_different_beta <- function(beta_values, network, static = TRUE, n_runs = 10, mc.cores = detectCores() - 1) {
  # Function to perform n_runs for a single beta
  simulate_single_beta <- function(beta) {
    
    # Run n_runs times using mclapply
    results <- mclapply(1:n_runs, function(run) {
      if (static) {
        df <- simulate_seir_static(network, beta = beta)
      } else {
        df <- simulate_seir_temporal(network, beta = beta)
      }
      
      R_final <- df[nrow(df), "R"]
      N <- sum(df[nrow(df), c("S", "E", "I", "R")])
      
      return(R_final / N)
    }, mc.cores = mc.cores)
    
    # Unlist results and calculate mean and standard deviation
    results <- unlist(results)
    
    return(c(beta = beta, 
             avg_frac_recov = mean(results), 
             std_frac_recov = sd(results) / sqrt(n_runs)))
  }
  
  # Apply the function over all beta values
  final_results <- lapply(beta_values, simulate_single_beta)
  
  # Combine results into a dataframe
  final_results_df <- do.call(rbind, final_results)
  
  return(as.data.frame(final_results_df))
}
