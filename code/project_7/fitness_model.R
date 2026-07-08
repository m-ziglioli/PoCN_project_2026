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

attraction_probabilities <- function(g) {
    # creating exception for first node, k1 = 0
    # all new m links will be connected to node k1
    if (vcount(g) == 1) {
        return(1)
    }
    else {
        V(g)$fitness *degree(g) / sum(V(g)$fitness * degree(g))
    }
}

create_links <- function(g, m, attach_prob) {
    
    # sample m nodes among the old nodes V(g)[1:(N-1)]
    # and attach the new links to them
    z <- length(V(g)) - 1
    
    # distinguish if m<z there are enough old nodes to make 
    # m new connections
    if (m < z ) {
        idxs <- sample(1:z, size=m, replace=FALSE, 
                    prob=attach_prob)
    }
    else {
        idxs <- sample(1:z, size=m, replace=TRUE, 
                    prob=attach_prob)
    }

    chosen_nodes <- V(g)[idxs]
    new_node <- V(g)[vcount(g)]

    for (j in seq_along(chosen_nodes)) {
        old_node <- chosen_nodes[j]
        g <- add_edges(g, c(new_node, old_node))
    }
    
    return(g)

    
}

make_fitness_graph <- function(N,m=1, dt=1, theta=1,  beta=1) {
    # creating first node
    # at time 1, at time 0 no nodes present
    g <- make_graph(edges=c(), n=1, directed=FALSE)

    V(g)$fitness <- draw_fitness(theta, beta)

    for (t in seq(2, N, dt)) {
        nodes_attraction_prob <- attraction_probabilities(g)

        g <- add_node(g, theta, beta)
        g <- create_links(g, m, attach_prob=nodes_attraction_prob)

    }
    #print(vcount(g))
    #print(V(g)$fitness)

    return(g)
}

#make_fitness_graph(N=10)