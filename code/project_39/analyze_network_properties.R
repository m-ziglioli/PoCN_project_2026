
library(data.table)
library(igraph)
library(ggplot2)

# Helper function to plot degree distribution
plot_degree_dist <- function(g, filename, title_text = "Degree Distribution") {
  deg <- degree(g)
  df_plot <- data.frame(degree = deg)
  
  p <- ggplot(df_plot, aes(x = degree)) +
    geom_histogram(fill = "steelblue", color = "black", alpha = 0.8, bins = 30) +
    labs(title = title_text, x = "Degree (k)", y = "Count") +
    theme_classic(base_size = 18) +
    theme(
      text = element_text(face = "bold", size = 18),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 15)),
      axis.title = element_text(face = "bold", size = 18),
      axis.text = element_text(face = "bold", size = 18)
    )
  
  ggsave(filename, plot = p, width = 7, height = 6, dpi = 300, units = "in")
}

# Helper function to plot strength distribution
plot_strength_dist <- function(g, filename, title_text = "Strength Distribution") {
  # Node strength is calculated using the edge weight attribute
  str <- strength(g, weights = E(g)$weight)
  df_plot <- data.frame(strength = str)
  
  p <- ggplot(df_plot, aes(x = strength)) +
    geom_histogram(fill = "forestgreen", color = "black", alpha = 0.8, bins = 30) +
    scale_x_log10() +
    labs(title = title_text, x = "Strength (Duration, log scale)", y = "Count") +
    theme_classic(base_size = 18) +
    theme(
      text = element_text(face = "bold", size = 18),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 15)),
      axis.title = element_text(face = "bold", size = 18),
      axis.text = element_text(face = "bold", size = 18)
    )
  
  ggsave(filename, plot = p, width = 7, height = 6, dpi = 300, units = "in")
}

# Helper function to plot edge weight distribution
plot_weight_dist <- function(g, filename, title_text = "Edge Weight Distribution") {
  if (!is.null(E(g)$weight)) {
    w <- E(g)$weight
    df_plot <- data.frame(weight = w)
    
    p <- ggplot(df_plot, aes(x = weight)) +
      geom_histogram(fill = "purple", color = "black", alpha = 0.8, bins = 30) +
      scale_x_log10() +
      labs(title = title_text, x = "Edge Weight (Duration, log scale)", y = "Count") +
      theme_classic(base_size = 18) +
      theme(
        text = element_text(face = "bold", size = 18),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18, margin = margin(b = 15)),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(face = "bold", size = 18)
      )
    
    ggsave(filename, plot = p, width = 7, height = 6, dpi = 300, units = "in")
  }
}

# Function to analyze static network
analyze_static <- function(network_csv, output_dir = "../../data/project_39/figures", info_file = "../../data/project_39/network_info.txt") {
  df <- fread(network_csv)
  
  # Directly build graph. Your columns must be ordered: nodeID_from, nodeID_to, then metrics.
  g <- graph_from_data_frame(df, directed = FALSE)
  
  # Dynamically map your custom total duration column to igraph's native edge weight attribute
  if ("total_duration_over_sail" %in% edge_attr_names(g)) {
    E(g)$weight <- E(g)$total_duration_over_sail
  }
  
  num_nodes <- vcount(g)
  num_edges <- ecount(g)
  
  info_text <- sprintf("Static Network Analysis:\n  File: %s\n  Number of Nodes: %d\n  Number of Edges: %d\n", 
                       basename(network_csv), num_nodes, num_edges)
  cat(info_text)
  cat(info_text, file = info_file, append = FALSE)
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  plot_degree_dist(g, file.path(output_dir, "static_degree_distribution.png"), "Static Network Degree Distribution")
  plot_strength_dist(g, file.path(output_dir, "static_strength_distribution.png"), "Static Network Strength Distribution")
  plot_weight_dist(g, file.path(output_dir, "static_weight_distribution.png"), "Static Network Edge Weight Distribution")
  
  return(list(nodes = num_nodes, edges = num_edges))
}

# Function to analyze temporal network
analyze_temporal <- function(network_csv, output_dir = "../../data/project_39/figures", info_file = "../../data/project_39/network_info.txt") {
  df <- fread(network_csv)
  
  g <- graph_from_data_frame(df, directed = FALSE)
  
  # Match the weights for the temporal network if using daily durations
  if ("CUM_DURATION" %in% edge_attr_names(g)) {
    E(g)$weight <- E(g)$CUM_DURATION
  }
  
  num_nodes <- vcount(g)
  num_edges <- ecount(g)
  
  info_text <- sprintf("\nTemporal Network Analysis:\n  File: %s\n  Number of Nodes: %d\n  Number of Edges: %d\n", 
                       basename(network_csv), num_nodes, num_edges)
  cat(info_text)
  cat(info_text, file = info_file, append = TRUE)
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  plot_degree_dist(g, file.path(output_dir, "temporal_degree_distribution.png"), "Temporal Network Degree Distribution")
  plot_strength_dist(g, file.path(output_dir, "temporal_strength_distribution.png"), "Temporal Network Strength Distribution")
  plot_weight_dist(g, file.path(output_dir, "temporal_weight_distribution.png"), "Temporal Network Edge Weight Distribution")
  
  return(list(nodes = num_nodes, edges = num_edges))
}

# Execute automatically if run via Rscript

static_csv <- "../../data/project_39/static_network_sail_1.csv"
temporal_csv <- "../../data/project_39/temporal_network_sail_1.csv"
  
 
analyze_static(static_csv)
 
analyze_temporal(temporal_csv)
