source("common.R")
source("fitness_model.R")

####################################################
# computing mu

compute_partition_function <- function(g) {
    z <- V(g)$fitness * degree(g)
    return(sum(z))
}

make_run <- function(N, n_runs, m=1, theta=1, save=TRUE) {

    # per each beta, run n_runs to do statistical averages
    mu_mean <- numeric(length(beta_values))
    mu_std  <- numeric(length(beta_values))

    for (i in seq_along(beta_values)) {
        beta <- beta_values[i]

        mu_values <- replicate(n_runs, {
            g <- make_fitness_graph(N, beta = beta, m = m)
            Z_t <- compute_partition_function(g)
            Z_t / (m * vcount(g))
        })

        mu_mean[i] <- mean(mu_values)
        mu_std[i]  <- sd(mu_values)
    }

    results <- data.frame (
        beta_values = beta_values,
        mu_mean = mu_mean,
        mu_std = mu_std
    )

    # beta_values is global..

    if (save) {
        write.csv(results, file=paste0("../data/mu_values_N_", N))
    }

    return(results)
}

################ initial parameter values
m <- 1
theta <- 1
T_values <- seq(0.05, 10, length.out=20)
beta_values <- 1/T_values   

n_runs <- 10

res <- make_run(1000, n_runs=n_runs)

print(res)