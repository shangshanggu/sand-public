#!/usr/bin/env Rscript
# 05_plot_intervention_effects.R
#
# Generates the faceted intervention-effect plots reported in the thesis
# (Figures 8.1 and 8.2).  Each plot shows percentage reduction in the
# outcome (AUDIT-C mean or proportion scoring 5-12) as a function of
# population coverage, coloured by targeting strategy, faceted by
# intervention type (rows) × simulated wave (columns).
#
# Ported from zz_combinedAnalysis.R  analyse_and_plot_outcome()
#
# Outputs (per outcome × period × efficacy):
#   - reproduced/outputs/chapter8/figures/<outcome>_<period>_eff<efficacy>.png
#   - reproduced/outputs/chapter8/figures/<outcome>_<period>_eff<efficacy>.pdf

suppressPackageStartupMessages({
  library(yaml)
  library(jsonlite)
  library(dplyr)
  library(ggplot2)
  library(grid)
  library(gridExtra)
})

script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grep(file_arg, args)])
  if (length(script_path) == 0) return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  normalizePath(dirname(script_path[1]), winslash = "/", mustWork = TRUE)
}

.resolve_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  if (any(grepl("^--file=", args))) {
    return(normalizePath(file.path(script_dir(), "..", "..", "..", ".."),
                         winslash = "/", mustWork = TRUE))
  }
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (dir.exists(file.path(cwd, "reproduced"))) return(cwd)
  normalizePath(file.path(cwd, ".."), winslash = "/", mustWork = TRUE)
}
repo_root <- .resolve_repo_root()
source(file.path(repo_root, "reproduced", "R", "common.R"))

`%||%` <- function(x, y) if (is.null(x) || (is.character(x) && length(x) == 1L && !nzchar(x))) y else x

# ===========================================================================
# Colour palette and ordering (matches original code exactly)
# ===========================================================================
CUSTOM_COLOURS <- c(
  "IT-ID" = "#FF69B4",
  "IT-NC" = "#FF8C00",
  "ST-ID" = "#4169E1",
  "ST-NC" = "#8A2BE2",
  "CT-HD" = "#32CD32",
  "CT-RS" = "#FFD700"
)
CUSTOM_ORDER <- c("IT-ID", "IT-NC", "ST-ID", "ST-NC", "CT-HD", "CT-RS")

# Labels for facet strips
WAVE_LABELS <- c(
  "4" = "Intervention effective during freshers' month",
  "5" = "Intervention effective right after freshers' month",
  "6" = "Intervention effective before the second semester"
)
TYPE_LABELS <- c(
  "A" = "Brief Intervention",
  "B" = "Descriptive Norm Correction",
  "C" = "Peer Influence Resistance"
)

PERIOD_TITLES <- c(
  "within" = "after one period of simulation",
  "middle" = "at the beginning of the second semester",
  "final"  = "at the beginning of the second year"
)

# ===========================================================================
# Core plotting function
# ===========================================================================
plot_intervention_outcome <- function(df_analysis, outcome_col, outcome_name,
                                      period, efficacy, fig_dir) {
  plot_data <- df_analysis %>%
    dplyr::filter(intervention_efficacy == efficacy,
                  intervention_type != "C") %>%
    dplyr::mutate(
      intervention_targeting = factor(intervention_targeting, levels = CUSTOM_ORDER)
    )

  if (nrow(plot_data) == 0) {
    message(sprintf("  No data for efficacy=%s, skipping", efficacy))
    return(invisible(NULL))
  }

  period_desc <- PERIOD_TITLES[[period]] %||% period

  main_plot <- ggplot(plot_data,
    aes(x = intervention_proportion, y = percentage_reduction,
        colour = intervention_targeting, group = intervention_targeting)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3, shape = 21, fill = "white") +
    facet_grid(
      intervention_type ~ simulated_wave,
      labeller = labeller(
        simulated_wave = function(x) WAVE_LABELS[as.character(x)],
        intervention_type = function(x) TYPE_LABELS[as.character(x)]
      )
    ) +
    scale_x_continuous(
      breaks = unique(plot_data$intervention_proportion),
      labels = scales::percent_format(scale = 1)
    ) +
    scale_y_continuous(
      limits = c(-5, 30),
      labels = scales::percent_format(scale = 1)
    ) +
    scale_colour_manual(values = CUSTOM_COLOURS, breaks = CUSTOM_ORDER) +
    labs(
      title = sprintf("Percentage reduction in %s at %s%% efficacy strength, %s",
                       outcome_name, efficacy, period_desc),
      x = "Proportion of population targeted",
      y = sprintf("Percentage reduction in %s", outcome_name),
      colour = "Targeting Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "grey80", fill = NA),
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
    )

  # Legend annotation (matches original)
  legend_text <- paste0(
    "IT-ID: Individual Targeting - Highest Indegree; ",
    "IT-NC: Individual Targeting - Highest Network Centrality;\n",
    "ST-ID: Segmentation Targeting - Highest Indegree; ",
    "ST-NC: Segmentation Targeting - Highest Network Centrality;\n",
    "CT-HD: Control Targeting - Heaviest Drinkers; ",
    "CT-RS: Control Targeting - Random Selection"
  )
  annotation_plot <- ggplot() +
    geom_blank() +
    annotation_custom(
      textGrob(legend_text,
               x = 0.5, y = 0.7, hjust = 0.5, vjust = 0,
               gp = gpar(fontsize = 10, fontface = "italic", col = "grey50"))
    ) +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

  combined <- gridExtra::arrangeGrob(main_plot, annotation_plot,
                                      ncol = 1, heights = c(5, 1))

  # Sanitise outcome name for filename
  safe_name <- gsub("[^a-zA-Z0-9_]", "_", tolower(outcome_name))
  base_name <- sprintf("%s_%s_eff%s", safe_name, period, efficacy)

  ensure_parent_dir(file.path(fig_dir, "x"))
  png_path <- file.path(fig_dir, paste0(base_name, ".png"))
  pdf_path <- file.path(fig_dir, paste0(base_name, ".pdf"))

  ggsave(png_path, combined, width = 15, height = 10, units = "in", dpi = 300)
  ggsave(pdf_path, combined, width = 15, height = 10, units = "in")

  message(sprintf("  Saved %s", basename(png_path)))
  invisible(combined)
}

# ===========================================================================
# Main
# ===========================================================================
run <- function() {
  cfg <- yaml::read_yaml(file.path(repo_root, "reproduced", "config", "thesis.yml"))
  logs_root <- file.path(repo_root, cfg$project$paths$logs_dir %||% "reproduced/logs")
  ch8_out   <- file.path(repo_root, cfg$chapters$chapter8_interventions$outputs_dir
                          %||% "reproduced/outputs/chapter8")
  results_dir <- file.path(ch8_out, "results")
  fig_dir     <- file.path(ch8_out, "figures")

  # Outcomes to plot: AUDIT-C mean and proportion scoring 5-12
  outcomes <- list(
    list(col = "mean_value",  name = "AUDIT-C Score"),
    list(col = "n_5_to_12",  name = "AUDIT-C Score (5-12)")
  )

  # Periods to plot
  periods <- c("within", "final")
  # Add middle if the file exists
  if (file.exists(file.path(results_dir, "df_analysis_middle.csv"))) {
    periods <- c("within", "middle", "final")
  }

  plot_count <- 0L

  for (period in periods) {
    analysis_file <- file.path(results_dir, sprintf("df_analysis_%s.csv", period))
    if (!file.exists(analysis_file)) {
      message(sprintf("Skipping %s: %s not found", period, basename(analysis_file)))
      next
    }
    df <- read.csv(analysis_file, stringsAsFactors = FALSE)

    # Ensure composite column exists
    if (!"n_5_to_12" %in% names(df)) {
      df$n_5_to_12 <- df$n_5_to_8 + df$n_9_to_12
    }

    efficacies <- sort(unique(df$intervention_efficacy))
    message(sprintf("Plotting %s period (%d efficacy levels)", period, length(efficacies)))

    for (outcome in outcomes) {
      if (!outcome$col %in% names(df)) next
      for (eff in efficacies) {
        plot_intervention_outcome(df, outcome$col, outcome$name, period, eff, fig_dir)
        plot_count <- plot_count + 1L
      }
    }
  }

  # Log
  entry <- list(
    timestamp  = format_timestamp(),
    action     = "plot_intervention_effects",
    n_plots    = plot_count,
    periods    = periods,
    figure_dir = relative_repo_path(fig_dir, repo_root)
  )
  append_pipeline_log(logs_root, "chapter8", entry, history_key = "plotting")

  message(sprintf("Generated %d intervention plots → %s", plot_count, fig_dir))
}

run()
