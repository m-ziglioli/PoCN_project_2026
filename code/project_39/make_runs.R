library(parallel)
source("generate_surrogate_network.R")
source("epidemic_model_static.R")
source("epidemic_model_temporal.R")

simulate_real_networks <- function(beta_values, network, static = TRUE, n_runs = 10, mc.cores = max(1, parallel::detectCores() - 2)) {
  simulate_single_beta <- function(beta) {
    
    results <- lapply(1:n_runs, function(run) {
      tryCatch({
        if (static) {
          df <- simulate_seir_static(network, beta = beta)
        } else {
          df <- simulate_seir_temporal(network, beta = beta)
        }
        
        last_row <- df[nrow(df), c("S", "E", "I", "R")]
        N <- sum(as.numeric(last_row))
        
        c(frac_inf = as.numeric(last_row[["I"]]) / N,
          frac_recov = as.numeric(last_row[["R"]]) / N)
      }, error = function(e) {
        message(sprintf("beta=%s, run=%d failed: %s", beta, run, conditionMessage(e)))
        NULL
      })
    })
    
    # Rimuove i run falliti (NULL o errori interni al forking)
    results <- results[!sapply(results, function(x) is.null(x) || inherits(x, "try-error"))]
    
    if (length(results) == 0) {
      warning(sprintf("Tutti i run sono falliti per beta=%s", beta))
      return(c(beta = beta, avg_frac_inf = NA, std_frac_inf = NA,
               avg_frac_recov = NA, std_frac_recov = NA))
    }
    
    results_mat <- do.call(rbind, results)
    n_successful <- nrow(results_mat)
    
    return(c(beta = beta, 
             avg_frac_inf = mean(results_mat[, "frac_inf"]),
             std_frac_inf = sd(results_mat[, "frac_inf"]) / sqrt(n_successful),
             avg_frac_recov = mean(results_mat[, "frac_recov"]), 
             std_frac_recov = sd(results_mat[, "frac_recov"]) / sqrt(n_successful)))
  }
  
  final_results <- lapply(beta_values, simulate_single_beta)
  final_results_df <- do.call(rbind, final_results)
  
  return(as.data.frame(final_results_df))
}

simulate_surrogate_networks <- function(beta_values, network_type, n_runs = 10, mc.cores = max(1, parallel::detectCores() - 2), temporal_nw = NULL, static_nw = NULL) {

  if (network_type == "ER_temporal") {
    generate_network <- function() generate_ER_temporal(temporal_nw)
    static <- FALSE
  } else {
    generate_network <- function() generate_ER_static(static_nw)
    static <- TRUE
  }

  simulate_single_beta <- function(beta) {
    
    results <- lapply(1:n_runs, function(run) {
      tryCatch({
        network <- generate_network()
        
        if (static) {
          df <- simulate_seir_static(network, beta = beta)
        } else {
          df <- simulate_seir_temporal(network, beta = beta)
        }
        
        last_row <- df[nrow(df), c("S", "E", "I", "R")]
        N <- sum(as.numeric(last_row))
        
        c(frac_inf = as.numeric(last_row[["I"]]) / N,
          frac_recov = as.numeric(last_row[["R"]]) / N)
      }, error = function(e) {
        message(sprintf("beta=%s, run=%d failed: %s", beta, run, conditionMessage(e)))
        NULL
      })
    })
    
    results <- results[!sapply(results, function(x) is.null(x) || inherits(x, "try-error"))]
    
    if (length(results) == 0) {
      warning(sprintf("Tutti i run sono falliti per beta=%s", beta))
      return(c(beta = beta, avg_frac_inf = NA, std_frac_inf = NA,
               avg_frac_recov = NA, std_frac_recov = NA))
    }
    
    results_mat <- do.call(rbind, results)
    n_successful <- nrow(results_mat)
    
    return(c(beta = beta, 
             avg_frac_inf = mean(results_mat[, "frac_inf"]),
             std_frac_inf = sd(results_mat[, "frac_inf"]) / sqrt(n_successful),
             avg_frac_recov = mean(results_mat[, "frac_recov"]), 
             std_frac_recov = sd(results_mat[, "frac_recov"]) / sqrt(n_successful)))
  }
  
  final_results <- lapply(beta_values, simulate_single_beta)
  final_results_df <- do.call(rbind, final_results)
  
  return(as.data.frame(final_results_df))
}


# Executing runs

beta_values <- c(0.05, 0.1, 0.2, 0.25, 0.5, 1)
save_path <- "../../data/project_39/"

static_nw <- read.csv("../../data/project_39/static_network_sail_1.csv")
temporal_nw <-  read.csv("../../data/project_39/temporal_network_sail_1.csv")

#df <- simulate_real_networks(beta_values, temporal_nw, static=FALSE, n_runs=30)
#write.csv(df, file=paste0(save_path, "epidemic_real_temporal.csv"), row.names=FALSE)

#df <- simulate_real_networks(beta_values, static_nw, static=TRUE, n_runs=30)
#write.csv(df, file=paste0(save_path, "epidemic_real_aggregate.csv"), row.names=FALSE)

df <- simulate_surrogate_networks(beta_values, network_type="ER_temporal", n_runs=1, temporal_nw=temporal_nw)
write.csv(df, file=paste0(save_path, "epidemic_ER_temporal.csv"), row.names=FALSE)

df <- simulate_surrogate_networks(beta_values, network_type="ER_static", n_runs=1, static_nw=static_nw)
write.csv(df, file=paste0(save_path, "epidemic_ER_static.csv"), row.names=FALSE)