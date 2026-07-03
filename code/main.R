source("common.R")
source("fitness_model.R")

####################################################
# computing mu

compute_partition_function <- function(g) {
    z <- V(g)$fitness * degree(g)
    return(sum(z))
}

make_run <- function(N, n_runs, m=1, theta=1) {
    mu_mean <- c()
    mu_std <- c()

    # per each beta, run n_runs to do statistical averages
    for (beta in beta_values) {

        for (j in seq(1, n_runs)) {
        g <- make_fitness_graph(N, beta=beta, m=m)
        Z_t <- compute_partition_function(g)

        mu <- Z_t / (m * vcount(g) ) # N=vcount
        }

        
    }


}

g <- make_fitness_graph(N=100)

################ initial parameter values
m <- 1
theta <- 1
beta_values <- seq(0.1, 50, length.out=50)

n_runs <- 100


# for

