
####################################################
# computing mu

compute_partition_function <- function(g) {
    z <- V(g)$fitness * degree(g)
    return(sum(z))
}

make_run <- function(N, n_runs, m=1, theta=1, save=TRUE) {

    run_single_simulation <- function(beta, N, m) {
        # These are the steps that create the graph and compute the partition function
        g <- make_fitness_graph(N, beta = beta, m = m)
        Z_t <- compute_partition_function(g)
       
        alpha <- Z_t / (m * vcount(g))
        mu <- - 1/beta * log(alpha)

        # giant connected component
        gcc <- max(degree(g)) / N

        return(c(mu, gcc))
    }


   results <- lapply(beta_values, function(beta) {
    # Run parallel simulations for the current beta
    sim_list <- mclapply(1:n_runs, function(i) {
        run_single_simulation(beta, N, m)
    }, mc.cores = num_cores)        
    
    sim_matrix <- do.call(rbind, sim_list)

    # 3. Extract the individual columns
    mu_values  <- sim_matrix[, 1]
    gcc_values <- sim_matrix[, 2]
    
    # 4. Return calculated statistics for this beta
    c(
        mu_mean  = mean(mu_values), 
        mu_std   = sd(mu_values),       
        gcc_mean = mean(gcc_values),
        gcc_std  = sd(gcc_values)    
    )
})

# 5. Combine the global beta_values with the summarized rows
results_df <- data.frame(T_values = 1/beta_values, do.call(rbind, results))

    if (save) {
        write.csv(results_df, file=paste0("../data/mu_values_N_", N, ".csv"), row.names=FALSE)
    }

    return(results_df)
}

################################

# sources
source("common.R")
source("fitness_model.R")


################ initial parameter values
m <- 1
theta <- 1
T_values <- c(seq(0.03, 0.3, length.out=15)  , seq(0.3, 1.2, length.out=20), seq(1.2, 10, length.out=10))
beta_values <- 1/T_values
n_runs <- 30
#################################################
# PARALLELIZATION

num_cores <- detectCores() - 2
cl <- makeCluster(num_cores)

# CRITICAL: Export your functions and any global variables the cluster needs
# Replace 'igraph' with whatever package contains make_fitness_graph if applicable

res <- make_run(1000, n_runs=n_runs)

print(res)