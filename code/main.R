
####################################################
# computing mu

compute_partition_function <- function(g) {
    z <- V(g)$fitness * degree(g)
    return(sum(z))
}

make_run <- function(N, n_runs, m=1, theta=1, save=TRUE) {
    mu_mean <- c()
    mu_std <- c()

    run_single_simulation <- function(beta, N, m) {
        # These are the steps that create the graph and compute the partition function
        g <- make_fitness_graph(N, beta = beta, m = m)
        Z_t <- compute_partition_function(g)
        return(Z_t / (m * vcount(g)))
    }


    results <- lapply(beta_values, function(beta) {
        # mclapply automatically copies the sourced functions to the workers
        mu_values <- unlist(mclapply(1:n_runs, function(i) {
            run_single_simulation(beta, N, m)
        }, mc.cores = num_cores))
        
        c(mu_mean = mean(mu_values), mu_std = sqrt(var(mu_values)))
        })

    # beta_values is global..
    results_df <- data.frame(T_values = 1/beta_values, do.call(rbind, results))

    if (save) {
        write.csv(results_df, file=paste0("../data/mu_values_N_", N, ".csv"), row.names=FALSE)
    }

    return(results_df)
}

################################à

# sources
source("common.R")
source("fitness_model.R")
################ initial parameter values



m <- 1
theta <- 1
a <-seq(0.05, 1, length.out=20)
T_values <- c(a, seq(1, 5, length.out=20))
beta_values <- 1/T_values
n_runs <- 10
#################################################
# PARALLELIZATION

num_cores <- detectCores() - 2
cl <- makeCluster(num_cores)

# CRITICAL: Export your functions and any global variables the cluster needs
# Replace 'igraph' with whatever package contains make_fitness_graph if applicable

res <- make_run(1000, n_runs=n_runs)

print(res)