source('cruise_load_library.R')

# Load data using relative paths from code/project_39/
load('../../data/project_39/dataNodes.RData')
load('../../data/project_39/dataContact.RData')

# Extract sail of interest (sail 1)
sail <- 1
edgeList = copy(dataContact[SAIL==sail])
dataNodes = dataNodes[SAIL==sail]

###################################
# Cleaning and aggregating nodes
cleaned_nodes <- dataNodes[, .(
    ID = ID,
    COHORT = COHORT,
    TYPE = TYPE,
    SAIL = SAIL
)]

write.csv(cleaned_nodes, "../../data/project_39/nodeList.csv", row.names=FALSE)

# --- Time Aggregated Static Network ---
# Reconstruct a time-aggregated static network of the 4 days of the cruise.
# Columns: nodeID_from, nodeID_to, total_contact_duration, number_of_contacts
# total_contact_duration = sum of contact time between the nodes
# number_of_contacts = number of contacts between the two nodes across the sailing

# setting two weights: total contact durations and number of contacts occured

edgeList = edgeList[, .(CUM_DURATION = sum(DURATION)), by = .(ID.x, ID.y, DAY_INTERACT)]
edgeList = edgeList[order(ID.x, ID.y, DAY_INTERACT)]

# 3 & 4. Generate the final static network
# This directly calculates total duration and counts total distinct contacts
static_net= edgeList[, .(
  duration = sum(CUM_DURATION)
  #number_of_contacts       = .N
), by = .(nodeID_from = ID.x, nodeID_to = ID.y)]

# Save static network as CSV
write.csv(static_net, '../../data/project_39/static_network_sail_1.csv', row.names = FALSE)

# --- Temporal Network ---
# Reconstruct a temporal network.
# Columns: time, nodeID_from, nodeID_to, contact_duration, location_or_setting_if_available
edgeList = copy(dataContact[SAIL==sail])

temporal_net <- edgeList[, .(
  DAY_INTERACT = DAY_INTERACT,
  nodeID_from = ID.x,
  nodeID_to = ID.y,
  contact_duration = DURATION,
  contact_type = CONTACT_TYPE
)]

# Save temporal network as CSV
write.csv(temporal_net, '../../data/project_39/temporal_network_sail_1.csv', row.names = FALSE)

# Print verification info
cat("Reconstruction completed successfully!\n")
cat("Static network rows:", nrow(static_net), "\n")
cat("Temporal network rows:", nrow(temporal_net), "\n")


##################################################à
# Functions to generate temporal and static surrogates 
# of the network

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