source("common.R")
source("fitness_model.R")

####################################################
# computing mu

compute_partition_function <- function(g) {
    z <- V(g)$fitness * degree(g)
    return(sum(z))
}

make_run <- function(N, n_runs, m=1, theta=1, save=TRUE) {
    mu_mean <- c()
    mu_std <- c()

    # per each beta, run n_runs to do statistical averages
    for (beta in beta_values) {
        mu_values <-  c()

        for (j in seq(1, n_runs)) {
        g <- make_fitness_graph(N, beta=beta, m=m)
        Z_t <- compute_partition_function(g)

        mu <- Z_t / (m * vcount(g) ) # N=vcount
        mu_values <- c(mu_values, mu)
        }

        # appending results for this beta
        mu_mean <- c(mu_mean, mean(mu_values))
        mu_std <- c(mu_std, sqrt(var(mu_values)))
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
beta_values <- c(1, 1)#seq(0.1, 50, length.out=50)

n_runs <- 1

res <- make_run(1000, n_runs=n_runs)

print(res)