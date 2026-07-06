source('cruise_load_library.R')

# Load data using relative paths from code/project_39/
load('../../data/project_39/dataNodes.RData')
load('../../data/project_39/dataContact.RData')

# Extract sail of interest (sail 1)
sail <- 1
edgeList <- dataContact[SAIL == sail]
dataNodes <- dataNodes[SAIL == sail]

# --- Time Aggregated Static Network ---
# Reconstruct a time-aggregated static network of the 4 days of the cruise.
# Columns: nodeID_from, nodeID_to, total_contact_duration, number_of_contacts
# total_contact_duration = sum of contact time between the nodes
# number_of_contacts = number of contacts between the two nodes across the sailing
static_net <- edgeList[, .(
  total_contact_duration = sum(DURATION),
  number_of_contacts = .N
), by = .(nodeID_from = ID.x, nodeID_to = ID.y)]

# Save static network as CSV
write.csv(static_net, '../../data/project_39/static_network_sail_1.csv', row.names = FALSE)

# --- Temporal Network ---
# Reconstruct a temporal network.
# Columns: time, nodeID_from, nodeID_to, contact_duration, location_or_setting_if_available
temporal_net <- edgeList[, .(
  time = DAY_INTERACT,
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
