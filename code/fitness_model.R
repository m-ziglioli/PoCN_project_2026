source("common.R")

#
#
#
#
#
#

draw_fitness <- function(theta=1, beta) {
    # defining eps_max and normalizing constant
    eps_max <- 1
    C <- (theta+1) / eps_max^(theta+1)

    # Using inverse cumulative funct to sample
    # from g(epsilon) = C e^theta
    u <- runif(1)
    eps <- (u)^(1/(theta+1)) * eps_max

    # being epsilon=-1/beta * log(eta)
    # return eta
    eta <- exp(-beta*eps)

    return(eta)
}

add_node <- function(g, theta, beta) {
    # adding one node and setting its fitness
    # by accesing last node
    g <- add_vertices(g, 1)
    V(g)$fitness[vcount(g)] <- draw_fitness(theta, beta)

    return(g)
}

create_links <- function(g)

main <- function(N,m=1, dt=1, theta=1,  beta=1) {
    # creating first node
    # at time 1, at time 0 no nodes present
    g <- make_graph(edges=c(), n=1)

    V(g)$fitness <- draw_fitness(theta, beta)

    for (t in seq(2, N, dt)):
        g <- add_node(g, theta, beta)

        g <- create_links(g, m)

}
