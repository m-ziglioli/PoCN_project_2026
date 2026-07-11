##################################################à
# Functions to generate temporal and static surrogates 
# of the network
library(igraph)

generate_ER_temporal <- function() {
  # preserving number of nodes and contacts,
  # while randomizing interacting pairs

  temporal_nw <- read.csv("../../data/project_39/temporal_network_sail_1.csv")

  nodes <- unique(c(temporal_nw$node_from, temporal_nw$node_to))
  n <- nrow(temporal_nw)

  rand_pairs <- data.frame()

  while (nrow(rand_pairs) < n) {
  tmp <- data.frame(
      node_from = sample(nodes, n, replace = TRUE),
      node_to = sample(nodes, n, replace = TRUE)
  )
  tmp <- tmp[tmp$node_from != tmp$node_to, ]
  rand_pairs <- rbind(rand_pairs, tmp)
  }

  rand_pairs <- rand_pairs[1:n, ]

  temporal_nw$node_from <- rand_pairs$node_from
  temporal_nw$node_to <- rand_pairs$node_to

  return(temporal_nw)
}

generate_ER_static <- function() {
  # erdos-renyi model preserving number of nodes and edges of the aggregated static network,
  # reshuffling the weights

  nodes <- read.csv("../../data/project_39/nodeList.csv")
  N <- nrow(nodes)

  static_nw <- read.csv("../../data/project_39/static_network_sail_1.csv")
  edges <- nrow(static_nw)

  g<-erdos.renyi.game(n=N, p.or.m= edges, type="gnm" ,loops = FALSE)

  # Extract edge list (vertex numbers)
  el <- as.data.frame(get.edgelist(g))
  names(el) <- c("node_from", "node_to")

  # Substitute into original dataframe
  static_nw$node_from <- el$node_from
  static_nw$node_to   <- el$node_to
  
  # Randomizing the interaction duration
  # by permutating the durations 
  static_nw$duration <- sample(static_nw$duration)

  return(static_nw)
    
}