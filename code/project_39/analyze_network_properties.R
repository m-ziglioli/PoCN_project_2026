source('cruise_load_library.R')

# Helper function to plot degree distribution
plot_degree_dist <- function(df, prefix = "static", output_dir = "../../data/project_39/figures") {
  g <- graph_from_data_frame(df, directed = FALSE)
  degrees <- degree(g)
  
  deg_df <- data.frame(degree = degrees)
  
  p <- ggplot(deg_df, aes(x = degree)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.8) +
    labs(title = paste(tools::toTitleCase(prefix), "Degree Distribution"), x = "Degree", y = "Count") +
    theme_classic(base_size = 14) + # Moderated base size for cleaner scaling
    theme(
      text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 15))
    )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- file.path(output_dir, paste0(prefix, "_degree_distribution.png"))
  
  # Changed dimensions to inches to work seamlessly with 300 DPI
  png(filename, width = 7, height = 6, units = "in", res = 300)
  print(p)
  dev.off()
}

# Helper function to plot strength distribution
plot_strength_dist <- function(df, prefix = "static", output_dir = "../../data/project_39/figures") {
  g <- graph_from_data_frame(df, directed = FALSE)
  
  weight_col <- NULL
  if ("total_contact_duration" %in% names(df)) {
    weight_col <- df$total_contact_duration
  } else if ("contact_duration" %in% names(df)) {
    weight_col <- df$contact_duration
  }
  
  strengths <- strength(g, weights = weight_col)
  str_df <- data.frame(strength = strengths)
  
  p <- ggplot(str_df, aes(x = strength)) +
    geom_histogram(fill = "forestgreen", color = "black", alpha = 0.8) +
    scale_x_log10() + 
    labs(title = paste(tools::toTitleCase(prefix), "Strength Distribution"), x = "Strength (Duration in s)", y = "Count") +
    theme_classic(base_size = 14) +
    theme(
      text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 15))
    )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- file.path(output_dir, paste0(prefix, "_strength_distribution.png"))
  
  # Changed dimensions to inches, removed pointsize override, set res to 300
  png(filename, width = 7, height = 6, units = "in", res = 300)
  print(p)
  dev.off()
}

# Function to analyze static network
analyze_static <- function(network_csv, static = TRUE, output_dir = "../../data/project_39/figures", info_file = "../../data/project_39/network_info.txt") {
  df <- fread(network_csv)
  
  nodes <- unique(c(df$nodeID_from, df$nodeID_to))
  num_nodes <- length(nodes)
  
  num_edges <- NA
  if (static) {
    num_edges <- nrow(df)
  }
  
  info_text <- sprintf("Static Network Analysis:\n  File: %s\n  Number of Nodes: %d\n  Number of Edges: %d\n", 
                       basename(network_csv), num_nodes, num_edges)
  cat(info_text)
  cat(info_text, file = info_file, append = FALSE)
  
  plot_degree_dist(df, prefix = "static", output_dir = output_dir)
  plot_strength_dist(df, prefix = "static", output_dir = output_dir)
  
  return(list(nodes = num_nodes, edges = num_edges))
}

analyzie_static <- analyze_static

# Function to analyze temporal network
analyze_temporal <- function(network_csv, output_dir = "../../data/project_39/figures", info_file = "../../data/project_39/network_info.txt") {
  df <- fread(network_csv)
  
  nodes <- unique(c(df$nodeID_from, df$nodeID_to))
  num_nodes <- length(nodes)
  
  contacts_per_day <- df[, .(num_contacts = .N, total_duration = sum(contact_duration)), by = time]
  contacts_per_day <- contacts_per_day[order(time)]
  contacts_per_day[, total_duration_nodes := 2 * total_duration]
  
  info_text <- sprintf("\nTemporal Network Analysis:\n  File: %s\n  Number of Nodes: %d\n  Daily Summary:\n", 
                       basename(network_csv), num_nodes)
  cat(info_text)
  cat(info_text, file = info_file, append = TRUE)
  
  for (i in 1:nrow(contacts_per_day)) {
    day_info <- sprintf("    Day %d: Contacts = %d, Total Duration of All Nodes = %.1f s\n", 
                        contacts_per_day$time[i], contacts_per_day$num_contacts[i], contacts_per_day$total_duration_nodes[i])
    cat(day_info)
    cat(day_info, file = info_file, append = TRUE)
  }
  
  aggregated_df <- df[, .(
    total_contact_duration = sum(contact_duration),
    number_of_contacts = .N
  ), by = .(nodeID_from, nodeID_to)]
  
  plot_degree_dist(aggregated_df, prefix = "temporal", output_dir = output_dir)
  plot_strength_dist(aggregated_df, prefix = "temporal", output_dir = output_dir)
  
  max_contacts <- max(contacts_per_day$num_contacts)
  max_duration <- max(contacts_per_day$total_duration_nodes)
  scale_factor <- if (max_contacts > 0) max_duration / max_contacts else 1
  if (scale_factor == 0 || is.na(scale_factor)) scale_factor <- 1
  
  p_temp <- ggplot(contacts_per_day, aes(x = time)) +
    geom_line(aes(y = num_contacts, color = "Number of Contacts"), size = 1.2) +
    geom_point(aes(y = num_contacts, color = "Number of Contacts"), size = 2.5) +
    geom_line(aes(y = total_duration_nodes / scale_factor, color = "Total Contact Duration (Nodes)"), size = 1.2, linetype = "dashed") +
    geom_point(aes(y = total_duration_nodes / scale_factor, color = "Total Contact Duration (Nodes)"), size = 2.5) +
    scale_y_continuous(
      name = "Number of Contacts per Day",
      sec.axis = sec_axis(~ . * scale_factor, name = "Total Contact Duration of All Nodes per Day (s)")
    ) +
    scale_color_manual(values = c(
      "Number of Contacts" = "royalblue",
      "Total Contact Duration (Nodes)" = "firebrick"
    )) +
    labs(title = "Temporal Activity Over Time", x = "Time (Days)", color = "Metric") +
    theme_classic(base_size = 14) +
    theme(
      text = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y.left = element_text(face = "bold", margin = margin(r = 10), color = "royalblue"),
      axis.title.y.right = element_text(face = "bold", margin = margin(l = 10), color = "firebrick"),
      axis.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 15)),
      legend.position = "bottom",
      legend.margin = margin(t = 10)
    )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- file.path(output_dir, "temporal_activity.png")
  
  # Adjusted dimensions to a nice wide landscape format for dual axis charts
  png(filename, width = 9, height = 6, units = "in", res = 300)
  print(p_temp)
  dev.off()
  
  return(list(nodes = num_nodes, daily_stats = contacts_per_day))
}

# Execute automatically if run via Rscript
if (!interactive()) {
  static_csv <- "../../data/project_39/static_network_sail_1.csv"
  temporal_csv <- "../../data/project_39/temporal_network_sail_1.csv"
  
  if (file.exists(static_csv)) {
    cat("Running static network analysis...\n")
    analyze_static(static_csv)
  } else {
    cat("Static CSV not found at:", static_csv, "\n")
  }
  
  if (file.exists(temporal_csv)) {
    cat("Running temporal network analysis...\n")
    analyze_temporal(temporal_csv)
  } else {
    cat("Temporal CSV not found at:", temporal_csv, "\n")
  }
}