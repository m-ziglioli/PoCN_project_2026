library(parallel)
source("generate_networks")

simulate_real_networks <- function(beta_values, network, static = TRUE, n_runs = 10, mc.cores = detectCores() - 1) {
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

simulate_surrogate_networks <- function(beta_values, network_type=c("ER_temporal, ER_static"), n_runs = 10, mc.cores = detectCores() - 1) {

  if (network_type == "ER_temporal") {
    generate_network <- generate_ER_temporal
    static <- FALSE
  }
  else{
    generate_network <- generate_ER_static
    static <- TRUE
  }


  # Function to perform n_runs for a single beta
  simulate_single_beta <- function(beta) {
    
    # Run n_runs times using mclapply
    results <- mclapply(1:n_runs, function(run) {
      # generating the surrogate network
      network <- generate_network()

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


# Executing runs

beta_values <- 1:10
save_path <- "../../data/project_39/"

static_nw <- read.csv("../../data/project_39/static_network_sail_1.csv")
temporal_nw <-  read.csv("../../data/project_39/temporal_network_sail_1.csv")

df <- simulate_real_networks(beta_values, temporal_nw, static=FALSE, nruns=30)
save(df, file=paste0(save_path, "epidemic_real_temporal.RData"))

df <- simulate_real_networks(beta_values, static_nw, static=TRUE, nruns=30)
save(df, file=paste0(save_path, "epidemic_aggregate.RData"))

df <- simulate_surrogate_networks(beta_values, network_type="ER_temporal", nruns=30)
save(df, file=paste0(save_path, "epidemic_temporal_surrogate"))

df <- simulate_real_networks(beta_values, network_type="ER_static", nruns=30)
save(df, file=paste0(save_path, "epidemic_static_surrogate"))