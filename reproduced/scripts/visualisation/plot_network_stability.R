#!/usr/bin/env Rscript
# ==============================================================================
# NETWORK STABILITY AND JACCARD INDEX VISUALISATION
# ==============================================================================
#
# Generates diagnostic plots for assessing network stability between consecutive
# waves — a critical prerequisite for SAOM estimation. RSiena requires Jaccard
# indices between 0.3–0.7 for reliable convergence.
#
# Panels:
#   1. Jaccard index between consecutive SAOM waves with convergence bands
#   2. Tie turnover decomposition (persisted / dissolved / new)
#   3. Hamming distance heatmap between all wave pairs
#
# Usage:
#   Rscript plot_network_stability.R [data_dir] [output_dir]
#
# ==============================================================================

suppressPackageStartupMessages({
  library(igraph)
  library(ggplot2)
  library(gridExtra)
  library(scales)
})

resolve_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("--file=", "", args[grepl("--file=", args)])
  if (length(file_arg) > 0) {
    return(normalizePath(file.path(dirname(file_arg), "..", ".."), mustWork = TRUE))
  }
  if (file.exists("config/thesis.yml")) return(normalizePath("."))
  if (file.exists("reproduced/config/thesis.yml")) return(normalizePath("reproduced"))
  stop("Cannot resolve repo root.")
}

load_list_by_wave <- function(data_dir) {
  path <- file.path(data_dir, "list_by_wave.RData")
  if (!file.exists(path)) stop("list_by_wave.RData not found at: ", path)
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  get("list_by_wave", envir = env)
}

# --- Tie set extraction ------------------------------------------------------

extract_tie_set <- function(wave_df, valid_ids) {
  if (!"nomination" %in% names(wave_df)) return(character(0))
  edges <- wave_df[!is.na(wave_df$nomination) &
                   wave_df$redcap_survey_identifier %in% valid_ids &
                   wave_df$nomination %in% valid_ids, ]
  paste(edges$redcap_survey_identifier, edges$nomination, sep = "->")
}

# --- Jaccard and turnover ---------------------------------------------------

compute_jaccard <- function(ties_a, ties_b) {
  ties_a <- unique(ties_a)
  ties_b <- unique(ties_b)
  if (length(ties_a) == 0 && length(ties_b) == 0) return(1.0)
  intersection <- length(intersect(ties_a, ties_b))
  union_size <- length(union(ties_a, ties_b))
  if (union_size == 0) return(0)
  intersection / union_size
}

compute_turnover <- function(ties_a, ties_b) {
  ties_a <- unique(ties_a)
  ties_b <- unique(ties_b)
  persisted <- length(intersect(ties_a, ties_b))
  dissolved <- length(setdiff(ties_a, ties_b))
  new_ties <- length(setdiff(ties_b, ties_a))
  list(persisted = persisted, dissolved = dissolved, new = new_ties,
       total_t1 = length(ties_a), total_t2 = length(ties_b))
}

# --- Plot 1: Jaccard index with convergence bands ----------------------------

plot_jaccard_indices <- function(jaccard_df, output_path) {
  p <- ggplot(jaccard_df, aes(x = transition, y = jaccard)) +
    # RSiena convergence bands
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.3, ymax = 0.7,
             alpha = 0.12, fill = "green3") +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.2, ymax = 0.3,
             alpha = 0.10, fill = "orange") +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.2,
             alpha = 0.10, fill = "red3") +
    # Reference lines
    geom_hline(yintercept = 0.3, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    geom_hline(yintercept = 0.7, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    # Data
    geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) +
    geom_text(aes(label = sprintf("%.3f", jaccard)), vjust = -0.5, size = 3.5, fontface = "bold") +
    # Annotations
    annotate("text", x = 0.5, y = 0.5, label = "Good for SAOM", size = 2.8,
             colour = "green4", hjust = 0, fontface = "italic") +
    annotate("text", x = 0.5, y = 0.25, label = "Marginal", size = 2.8,
             colour = "orange3", hjust = 0, fontface = "italic") +
    annotate("text", x = 0.5, y = 0.1, label = "Too unstable", size = 2.8,
             colour = "red4", hjust = 0, fontface = "italic") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(x = NULL, y = "Jaccard Index",
         title = "Network stability between consecutive SAOM waves",
         subtitle = "Jaccard index = |intersection| / |union| of tie sets; RSiena recommends 0.3\u20130.7") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 13, face = "bold"),
          panel.grid.major.x = element_blank())

  ggsave(output_path, p, width = 8, height = 5, dpi = 150)
  message("[viz] Jaccard indices saved to ", output_path)
}

# --- Plot 2: Tie turnover decomposition --------------------------------------

plot_tie_turnover <- function(turnover_df, output_path) {
  # Reshape for stacked bar
  long_df <- data.frame(
    transition = rep(turnover_df$transition, 3),
    type = rep(c("Persisted", "Dissolved", "New"), each = nrow(turnover_df)),
    count = c(turnover_df$persisted, turnover_df$dissolved, turnover_df$new),
    stringsAsFactors = FALSE
  )
  long_df$type <- factor(long_df$type, levels = c("Persisted", "Dissolved", "New"))

  p <- ggplot(long_df, aes(x = transition, y = count, fill = type)) +
    geom_col(position = "stack", alpha = 0.85, width = 0.6) +
    scale_fill_manual(values = c("Persisted" = "steelblue",
                                 "Dissolved" = "coral2",
                                 "New" = "seagreen3")) +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5),
              size = 3, colour = "white", fontface = "bold") +
    labs(x = NULL, y = "Number of ties", fill = NULL,
         title = "Tie turnover between consecutive waves",
         subtitle = "Decomposition: persisted ties + dissolved ties + newly formed ties") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 13, face = "bold"),
          legend.position = "bottom",
          panel.grid.major.x = element_blank())

  ggsave(output_path, p, width = 8, height = 5, dpi = 150)
  message("[viz] Tie turnover saved to ", output_path)
}

# --- Plot 3: Hamming distance heatmap ----------------------------------------

plot_hamming_heatmap <- function(tie_sets, wave_labels, all_ids, output_path) {
  n_waves <- length(tie_sets)
  n <- length(all_ids)
  max_possible <- n * (n - 1)  # directed network, no self-loops

  # Compute Hamming distance (proportion of differing dyads) between all pairs
  hamming_mat <- matrix(0, n_waves, n_waves)
  for (i in 1:n_waves) {
    for (j in 1:n_waves) {
      if (i == j) next
      all_dyads <- union(tie_sets[[i]], tie_sets[[j]])
      diff_count <- length(setdiff(tie_sets[[i]], tie_sets[[j]])) +
                    length(setdiff(tie_sets[[j]], tie_sets[[i]]))
      hamming_mat[i, j] <- diff_count / max_possible
    }
  }

  # Convert to long format for ggplot
  heatmap_df <- expand.grid(Wave_A = wave_labels, Wave_B = wave_labels)
  heatmap_df$distance <- as.vector(hamming_mat)
  heatmap_df$Wave_A <- factor(heatmap_df$Wave_A, levels = wave_labels)
  heatmap_df$Wave_B <- factor(heatmap_df$Wave_B, levels = rev(wave_labels))

  p <- ggplot(heatmap_df, aes(x = Wave_A, y = Wave_B, fill = distance)) +
    geom_tile(colour = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.4f", distance)), size = 3.5) +
    scale_fill_gradient(low = "white", high = "steelblue",
                        name = "Hamming\ndistance") +
    labs(x = NULL, y = NULL,
         title = "Pairwise Hamming distance between wave networks",
         subtitle = "Proportion of dyads that differ between waves (lower = more similar)") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 0),
          panel.grid = element_blank())

  ggsave(output_path, p, width = 7, height = 6, dpi = 150)
  message("[viz] Hamming heatmap saved to ", output_path)
}

# --- Main --------------------------------------------------------------------

main <- function() {
  repo_root <- resolve_repo_root()
  args <- commandArgs(trailingOnly = TRUE)

  data_dir <- if (length(args) >= 1) args[1] else file.path(repo_root, "data", "proxy")
  output_dir <- if (length(args) >= 2) args[2] else file.path(repo_root, "outputs", "visualisation")

  if (!dir.exists(data_dir)) stop("Data directory not found: ", data_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  message("=== Network Stability Diagnostics ===")
  list_by_wave <- load_list_by_wave(data_dir)
  all_ids <- sort(unique(list_by_wave[[1]]$redcap_survey_identifier))

  # SAOM-aligned waves
  saom_indices <- c(2, 4, 5, 6)
  wave_labels <- c("Wave 2", "Wave 4", "Wave 5", "Wave 6")

  # Extract tie sets
  tie_sets <- lapply(saom_indices, function(w) extract_tie_set(list_by_wave[[w]], all_ids))

  # Compute Jaccard and turnover for consecutive pairs
  transitions <- paste(wave_labels[-length(wave_labels)], "\u2192",
                       wave_labels[-1])

  jaccard_vals <- numeric(length(transitions))
  turnover_list <- vector("list", length(transitions))

  for (i in seq_along(transitions)) {
    jaccard_vals[i] <- compute_jaccard(tie_sets[[i]], tie_sets[[i + 1]])
    turnover_list[[i]] <- compute_turnover(tie_sets[[i]], tie_sets[[i + 1]])
  }

  jaccard_df <- data.frame(
    transition = factor(transitions, levels = transitions),
    jaccard = jaccard_vals,
    stringsAsFactors = FALSE
  )

  turnover_df <- data.frame(
    transition = factor(transitions, levels = transitions),
    persisted = sapply(turnover_list, `[[`, "persisted"),
    dissolved = sapply(turnover_list, `[[`, "dissolved"),
    new = sapply(turnover_list, `[[`, "new"),
    stringsAsFactors = FALSE
  )

  # Generate plots
  plot_jaccard_indices(jaccard_df, file.path(output_dir, "jaccard_indices.png"))
  plot_tie_turnover(turnover_df, file.path(output_dir, "tie_turnover.png"))
  plot_hamming_heatmap(tie_sets, wave_labels, all_ids,
                       file.path(output_dir, "hamming_heatmap.png"))

  # Save stats
  write.csv(jaccard_df, file.path(output_dir, "jaccard_indices.csv"), row.names = FALSE)
  write.csv(turnover_df, file.path(output_dir, "tie_turnover.csv"), row.names = FALSE)

  message("\n[viz] Jaccard indices:")
  print(jaccard_df)
  message("\n[viz] Tie turnover:")
  print(turnover_df)

  message(sprintf("\n=== Stability diagnostics saved to %s ===", output_dir))
}

if (identical(environment(), globalenv())) {
  tryCatch(main(), error = function(e) {
    message("Error generating stability diagnostics: ", e$message)
    quit(status = 1)
  })
}
