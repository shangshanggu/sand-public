#!/usr/bin/env Rscript
# ==============================================================================
# LONGITUDINAL NETWORK VISUALISATION PANEL
# ==============================================================================
#
# Generates a multi-panel figure showing the evolution of friendship networks
# across survey waves, with nodes coloured by drinking behaviour (AUDIT-C)
# and sized by degree centrality.
#
# Panels:
#   1. Network graphs for SAOM-aligned waves (2, 4, 5, 6) with consistent layout
#   2. Degree distribution evolution across waves
#   3. Drinking behaviour (AUDIT-C) distribution by wave
#   4. Network-behaviour association (mean alter AUDIT vs ego AUDIT)
#
# Usage:
#   Rscript plot_network_panel.R [data_dir] [output_dir]
#
# Defaults:
#   data_dir   = reproduced/data/proxy
#   output_dir = reproduced/outputs/visualisation
#
# ==============================================================================

suppressPackageStartupMessages({
  library(igraph)
  library(ggplot2)
  library(gridExtra)
  library(scales)
  library(RColorBrewer)
})

# --- Path resolution --------------------------------------------------------

resolve_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("--file=", "", args[grepl("--file=", args)])
  if (length(file_arg) > 0) {
    return(normalizePath(file.path(dirname(file_arg), "..", ".."), mustWork = TRUE))
  }
  # Fallback: if run from reproduced/ or repo root
  if (file.exists("config/thesis.yml")) return(normalizePath("."))
  if (file.exists("reproduced/config/thesis.yml")) return(normalizePath("reproduced"))
  stop("Cannot resolve repo root. Run from the repository root or reproduced/ directory.")
}

# --- Data loading ------------------------------------------------------------

load_list_by_wave <- function(data_dir) {
  path <- file.path(data_dir, "list_by_wave.RData")
  if (!file.exists(path)) stop("list_by_wave.RData not found at: ", path)
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  if (!"list_by_wave" %in% ls(env)) stop("list_by_wave object not found in RData file")
  get("list_by_wave", envir = env)
}

# --- Network construction ---------------------------------------------------

#' Build an igraph object from a single wave data frame
#' @param wave_df Data frame with redcap_survey_identifier, nomination, audit_score
#' @param all_ids Full set of participant IDs (for consistent node set)
#' @return igraph object with vertex attributes
build_wave_graph <- function(wave_df, all_ids) {
  # Extract edges (nomination ties)
  if ("nomination" %in% names(wave_df)) {
    edges <- wave_df[!is.na(wave_df$nomination),
                     c("redcap_survey_identifier", "nomination")]
    edges <- edges[edges$nomination %in% all_ids, ]
    names(edges) <- c("from", "to")
  } else {
    edges <- data.frame(from = integer(0), to = integer(0))
  }

  g <- graph_from_data_frame(edges, directed = TRUE, vertices = data.frame(name = all_ids))

  # Attach audit_score as vertex attribute
  unique_rows <- wave_df[!duplicated(wave_df$redcap_survey_identifier), ]
  audit_lookup <- setNames(unique_rows$audit_score, unique_rows$redcap_survey_identifier)
  V(g)$audit_score <- as.numeric(audit_lookup[as.character(V(g)$name)])

  # Degree centrality
  V(g)$in_degree <- degree(g, mode = "in")
  V(g)$out_degree <- degree(g, mode = "out")
  V(g)$total_degree <- degree(g, mode = "all")

  g
}

# --- Colour palette for AUDIT-C ---------------------------------------------

audit_palette <- function(scores, max_score = 12) {
  # Green (low risk) -> Yellow (moderate) -> Red (high risk)
  scores[is.na(scores)] <- 0
  scores <- pmin(scores, max_score)
  frac <- scores / max_score
  r <- ifelse(frac < 0.5, frac * 2, 1)
  g_col <- ifelse(frac < 0.5, 1, 1 - (frac - 0.5) * 2)
  b <- rep(0.1, length(scores))
  rgb(r, g_col, b, alpha = 0.85)
}

# --- Consistent layout across waves -----------------------------------------

compute_stable_layout <- function(graphs, seed = 42) {
  # Use the union of all edges to compute a single layout
  all_edges <- do.call(rbind, lapply(graphs, function(g) {
    el <- as_edgelist(g)
    if (nrow(el) == 0) return(data.frame(from = character(0), to = character(0)))
    data.frame(from = el[, 1], to = el[, 2], stringsAsFactors = FALSE)
  }))

  all_ids <- sort(unique(c(V(graphs[[1]])$name)))
  if (nrow(all_edges) > 0) {
    g_union <- graph_from_data_frame(all_edges, directed = TRUE,
                                     vertices = data.frame(name = all_ids))
  } else {
    g_union <- make_empty_graph(n = length(all_ids), directed = TRUE)
    V(g_union)$name <- all_ids
  }

  set.seed(seed)
  layout_with_fr(g_union, niter = 500)
}

# --- Panel 1: Network graphs ------------------------------------------------

plot_network_waves <- function(graphs, layout_mat, wave_labels, output_path) {
  n_waves <- length(graphs)

  png(output_path, width = 2400, height = 700, res = 150)
  par(mfrow = c(1, n_waves), mar = c(1, 1, 3, 1), bg = "white")

  for (i in seq_along(graphs)) {
    g <- graphs[[i]]
    scores <- V(g)$audit_score
    node_cols <- audit_palette(scores)
    node_sizes <- 2 + sqrt(V(g)$total_degree) * 1.5
    node_sizes[is.na(node_sizes)] <- 2

    plot(g,
         layout = layout_mat,
         vertex.size = node_sizes,
         vertex.color = node_cols,
         vertex.frame.color = adjustcolor("grey30", alpha.f = 0.5),
         vertex.frame.width = 0.3,
         vertex.label = NA,
         edge.arrow.size = 0.15,
         edge.color = adjustcolor("grey50", alpha.f = 0.25),
         edge.width = 0.4,
         main = wave_labels[i])

    # Network stats annotation
    density <- round(edge_density(g), 3)
    recip <- round(reciprocity(g), 2)
    mean_deg <- round(mean(degree(g, mode = "out")), 1)
    mtext(sprintf("density=%.3f  recip=%.2f  mean.deg=%.1f",
                  density, recip, mean_deg),
          side = 1, line = -0.5, cex = 0.65, col = "grey40")
  }

  dev.off()
  message("[viz] Network panel saved to ", output_path)
}

# --- Panel 2: Degree distributions ------------------------------------------

plot_degree_distributions <- function(graphs, wave_labels, output_path) {
  deg_data <- do.call(rbind, lapply(seq_along(graphs), function(i) {
    g <- graphs[[i]]
    data.frame(
      wave = wave_labels[i],
      in_degree = degree(g, mode = "in"),
      out_degree = degree(g, mode = "out"),
      stringsAsFactors = FALSE
    )
  }))
  deg_data$wave <- factor(deg_data$wave, levels = wave_labels)

  p_in <- ggplot(deg_data, aes(x = in_degree, fill = wave)) +
    geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7, colour = "white", linewidth = 0.2) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "In-degree (nominations received)", y = "Count",
         title = "In-degree distribution by wave") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          plot.title = element_text(size = 12, face = "bold"))

  p_out <- ggplot(deg_data, aes(x = out_degree, fill = wave)) +
    geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7, colour = "white", linewidth = 0.2) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "Out-degree (nominations made)", y = "Count",
         title = "Out-degree distribution by wave") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          plot.title = element_text(size = 12, face = "bold"))

  combined <- arrangeGrob(p_in, p_out, ncol = 2)
  ggsave(output_path, combined, width = 12, height = 5, dpi = 150)
  message("[viz] Degree distributions saved to ", output_path)
}

# --- Panel 3: AUDIT-C trajectories ------------------------------------------

plot_audit_trajectories <- function(list_by_wave, wave_indices, wave_labels, output_path) {
  audit_data <- do.call(rbind, lapply(seq_along(wave_indices), function(i) {
    w <- wave_indices[i]
    df <- list_by_wave[[w]]
    unique_df <- df[!duplicated(df$redcap_survey_identifier), ]
    data.frame(
      wave = wave_labels[i],
      audit_score = unique_df$audit_score,
      id = unique_df$redcap_survey_identifier,
      stringsAsFactors = FALSE
    )
  }))
  audit_data$wave <- factor(audit_data$wave, levels = wave_labels)

  # Summary stats for overlay
  summary_df <- aggregate(audit_score ~ wave, data = audit_data, FUN = function(x) {
    c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE), n = sum(!is.na(x)))
  })
  summary_df <- do.call(data.frame, summary_df)
  names(summary_df) <- c("wave", "mean", "median", "sd", "n")

  p <- ggplot(audit_data, aes(x = wave, y = audit_score)) +
    geom_violin(aes(fill = wave), alpha = 0.4, colour = NA, scale = "width") +
    geom_boxplot(width = 0.15, outlier.size = 0.8, outlier.alpha = 0.4,
                 fill = "white", alpha = 0.7) +
    geom_point(data = summary_df, aes(x = wave, y = mean),
               shape = 18, size = 3, colour = "red") +
    geom_line(data = summary_df, aes(x = wave, y = mean, group = 1),
              colour = "red", linewidth = 0.8, linetype = "dashed") +
    scale_fill_brewer(palette = "Set2") +
    labs(x = NULL, y = "AUDIT-C Score",
         title = "Drinking behaviour trajectories across waves",
         subtitle = "Red diamonds = wave means; dashed line = mean trajectory") +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 3,
             alpha = 0.08, fill = "green3") +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 8, ymax = Inf,
             alpha = 0.08, fill = "red3") +
    annotate("text", x = 0.55, y = 1.5, label = "Low risk", size = 2.5,
             colour = "green4", hjust = 0, fontface = "italic") +
    annotate("text", x = 0.55, y = 10, label = "High risk", size = 2.5,
             colour = "red4", hjust = 0, fontface = "italic") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          plot.title = element_text(size = 12, face = "bold"))

  ggsave(output_path, p, width = 8, height = 5, dpi = 150)
  message("[viz] AUDIT trajectories saved to ", output_path)
}

# --- Panel 4: Peer influence scatter -----------------------------------------

plot_peer_influence <- function(graphs, wave_labels, output_path) {
  scatter_data <- do.call(rbind, lapply(seq_along(graphs), function(i) {
    g <- graphs[[i]]
    ego_scores <- V(g)$audit_score
    # Calculate mean alter AUDIT score for each ego
    alter_means <- sapply(V(g), function(v) {
      neighbors <- neighbors(g, v, mode = "out")
      if (length(neighbors) == 0) return(NA_real_)
      mean(V(g)$audit_score[neighbors], na.rm = TRUE)
    })

    data.frame(
      wave = wave_labels[i],
      ego_audit = ego_scores,
      alter_mean_audit = alter_means,
      stringsAsFactors = FALSE
    )
  }))
  scatter_data$wave <- factor(scatter_data$wave, levels = wave_labels)
  scatter_data <- scatter_data[!is.na(scatter_data$alter_mean_audit) &
                               !is.na(scatter_data$ego_audit), ]

  p <- ggplot(scatter_data, aes(x = ego_audit, y = alter_mean_audit)) +
    geom_point(aes(colour = wave), alpha = 0.35, size = 1.2) +
    geom_smooth(method = "lm", se = TRUE, colour = "black",
                linewidth = 0.7, linetype = "solid", alpha = 0.15) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "grey50") +
    facet_wrap(~ wave, nrow = 1) +
    scale_colour_brewer(palette = "Set2") +
    labs(x = "Ego AUDIT-C score", y = "Mean alter AUDIT-C score",
         title = "Peer influence: ego vs mean friend drinking",
         subtitle = "Dotted line = perfect assortment; solid line = OLS fit") +
    coord_equal(xlim = c(0, 12), ylim = c(0, 12)) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold"),
          plot.title = element_text(size = 12, face = "bold"))

  ggsave(output_path, p, width = 14, height = 4, dpi = 150)
  message("[viz] Peer influence scatter saved to ", output_path)
}

# --- Panel 5: Community structure --------------------------------------------

plot_community_detection <- function(graphs, layout_mat, wave_labels, output_path) {
  n_waves <- length(graphs)

  png(output_path, width = 2400, height = 700, res = 150)
  par(mfrow = c(1, n_waves), mar = c(1, 1, 3, 1), bg = "white")

  community_palette <- c(brewer.pal(8, "Set2"), brewer.pal(8, "Pastel1"))

  for (i in seq_along(graphs)) {
    g <- graphs[[i]]
    g_undirected <- as.undirected(g, mode = "collapse")

    # Louvain community detection
    comm <- cluster_louvain(g_undirected)
    membership <- membership(comm)
    n_communities <- max(membership)
    mod <- round(modularity(comm), 3)

    node_cols <- community_palette[membership]
    node_sizes <- 2 + sqrt(V(g)$total_degree) * 1.5
    node_sizes[is.na(node_sizes)] <- 2

    plot(g,
         layout = layout_mat,
         vertex.size = node_sizes,
         vertex.color = adjustcolor(node_cols, alpha.f = 0.8),
         vertex.frame.color = adjustcolor("grey30", alpha.f = 0.4),
         vertex.frame.width = 0.3,
         vertex.label = NA,
         edge.arrow.size = 0.15,
         edge.color = adjustcolor("grey50", alpha.f = 0.2),
         edge.width = 0.4,
         main = wave_labels[i])

    mtext(sprintf("communities=%d  modularity=%.3f", n_communities, mod),
          side = 1, line = -0.5, cex = 0.65, col = "grey40")
  }

  dev.off()
  message("[viz] Community detection panel saved to ", output_path)
}

# --- Panel 6: Network summary statistics table -------------------------------

compute_network_stats <- function(graphs, wave_labels) {
  stats <- do.call(rbind, lapply(seq_along(graphs), function(i) {
    g <- graphs[[i]]
    g_undirected <- as.undirected(g, mode = "collapse")
    comm <- cluster_louvain(g_undirected)

    data.frame(
      Wave = wave_labels[i],
      Nodes = vcount(g),
      Edges = ecount(g),
      Density = round(edge_density(g), 4),
      Reciprocity = round(reciprocity(g), 3),
      Transitivity = round(transitivity(g_undirected, type = "global"), 3),
      Mean_In_Degree = round(mean(degree(g, mode = "in")), 2),
      SD_In_Degree = round(sd(degree(g, mode = "in")), 2),
      Mean_AUDIT = round(mean(V(g)$audit_score, na.rm = TRUE), 2),
      Communities = max(membership(comm)),
      Modularity = round(modularity(comm), 3),
      stringsAsFactors = FALSE
    )
  }))
  stats
}

# --- Main --------------------------------------------------------------------

main <- function() {
  repo_root <- resolve_repo_root()
  args <- commandArgs(trailingOnly = TRUE)

  data_dir <- if (length(args) >= 1) args[1] else file.path(repo_root, "data", "proxy")
  output_dir <- if (length(args) >= 2) args[2] else file.path(repo_root, "outputs", "visualisation")

  if (!dir.exists(data_dir)) stop("Data directory not found: ", data_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  message("=== Longitudinal Network Visualisation ===")
  message("Data source: ", data_dir)
  message("Output directory: ", output_dir)

  # Load data
  list_by_wave <- load_list_by_wave(data_dir)
  message(sprintf("Loaded %d waves", length(list_by_wave)))

  # SAOM-aligned waves: 2, 4, 5, 6 (thesis timepoints)
  saom_wave_indices <- c(2, 4, 5, 6)
  wave_labels <- c("Wave 2\n(Oct-22)", "Wave 4\n(Dec-22)",
                    "Wave 5\n(Mar-23)", "Wave 6\n(Oct-23)")
  wave_labels_short <- c("Wave 2", "Wave 4", "Wave 5", "Wave 6")

  # Get consistent ID set from baseline
  all_ids <- sort(unique(list_by_wave[[1]]$redcap_survey_identifier))
  message(sprintf("Baseline participants: %d", length(all_ids)))

  # Build igraph objects for each SAOM wave
  graphs <- lapply(saom_wave_indices, function(w) {
    build_wave_graph(list_by_wave[[w]], all_ids)
  })

  # Compute stable layout across all waves
  message("Computing stable Fruchterman-Reingold layout...")
  layout_mat <- compute_stable_layout(graphs, seed = 42)

  # Generate all panels
  message("\nGenerating visualisation panels...")

  plot_network_waves(graphs, layout_mat, wave_labels,
                     file.path(output_dir, "network_panel_audit.png"))

  plot_community_detection(graphs, layout_mat, wave_labels,
                           file.path(output_dir, "network_panel_communities.png"))

  plot_degree_distributions(graphs, wave_labels_short,
                            file.path(output_dir, "degree_distributions.png"))

  plot_audit_trajectories(list_by_wave, saom_wave_indices, wave_labels_short,
                          file.path(output_dir, "audit_trajectories.png"))

  plot_peer_influence(graphs, wave_labels_short,
                      file.path(output_dir, "peer_influence_scatter.png"))

  # Summary statistics table
  stats <- compute_network_stats(graphs, wave_labels_short)
  write.csv(stats, file.path(output_dir, "network_summary_stats.csv"), row.names = FALSE)
  message("\n[viz] Network summary statistics:")
  print(stats)

  message(sprintf("\n=== All visualisations saved to %s ===", output_dir))
}

if (identical(environment(), globalenv())) {
  tryCatch(main(), error = function(e) {
    message("Error generating visualisations: ", e$message)
    quit(status = 1)
  })
}
