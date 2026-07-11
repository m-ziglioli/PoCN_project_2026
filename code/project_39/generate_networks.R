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
  duration = round(sum(CUM_DURATION) / 60, digits=3) # duration in minutes
  #number_of_contacts       = .N
), by = .(node_from = ID.x, node_to = ID.y)]

# Save static network as CSV
write.csv(static_net, '../../data/project_39/static_network_sail_1.csv', row.names = FALSE)

# --- Temporal Network ---
# Reconstruct a temporal network.
# Columns: time, nodeID_from, nodeID_to, contact_duration, location_or_setting_if_available
edgeList = copy(dataContact[SAIL==sail])

temporal_net <- edgeList[, .(
  node_from = ID.x,
  node_to = ID.y,
  DAY_INTERACT = DAY_INTERACT,
  contact_duration = round(DURATION /60, digits=3),
  contact_type = CONTACT_TYPE
)]

# Save temporal network as CSV
write.csv(temporal_net, '../../data/project_39/temporal_network_sail_1.csv', row.names = FALSE)

# Print verification info
cat("Reconstruction completed successfully!\n")
cat("Static network rows:", nrow(static_net), "\n")
cat("Temporal network rows:", nrow(temporal_net), "\n")


