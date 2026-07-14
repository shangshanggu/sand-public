#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- grep(file_arg, cmd_args)
  if (length(matches) > 0) {
    return(normalizePath(sub(file_arg, "", cmd_args[matches[1]]), winslash = "/", mustWork = TRUE))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE))
  }
  stop("Unable to determine script path for Chapter 5 figure generation.")
}

script_dir <- dirname(get_script_path())
repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

resolve_chapter5_outputs <- function(bundle) {
  resolve_chapter_output_paths(bundle, "chapter5_descriptive_norms")
}

load_summary <- function(paths) {
  summary_path <- file.path(paths$tables, "nam_summary.csv")
  if (!file.exists(summary_path)) {
    stop(sprintf("Missing NAM summary at %s. Run 02_estimate_nam_models.R first.", summary_path))
  }
  list(data = read.csv(summary_path, stringsAsFactors = FALSE), path = summary_path)
}

prepare_plot_data <- function(summary_df) {
  # Support both old format (term column) and new lnam format (term_raw column)
  if ("term_raw" %in% names(summary_df) && !"term" %in% names(summary_df)) {
    term_map <- c(
      "misperception_audit_c_global"   = "global_misperception",
      "misperception_audit_c_peer"     = "peer_misperception",
      "misperception_audit_score_peer" = "peer_misperception"
    )
    summary_df$term <- term_map[summary_df$term_raw]
  }

  keep <- !is.na(summary_df$term) & summary_df$term %in% c("global_misperception", "peer_misperception")
  filtered <- summary_df[keep, c("time_period", "term", "estimate"), drop = FALSE]
  if (nrow(filtered) == 0) {
    stop("No coefficient data available for plotting.")
  }
  periods <- unique(filtered$time_period)
  periods <- periods[order(periods)]
  plot_matrix <- data.frame(
    time_period = periods,
    global = NA_real_,
    peer = NA_real_,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(periods)) {
    period <- periods[i]
    global_row <- filtered[filtered$time_period == period & filtered$term == "global_misperception", , drop = FALSE]
    peer_row <- filtered[filtered$time_period == period & filtered$term == "peer_misperception", , drop = FALSE]
    if (nrow(global_row) == 1) {
      plot_matrix$global[i] <- global_row$estimate
    }
    if (nrow(peer_row) == 1) {
      plot_matrix$peer[i] <- peer_row$estimate
    }
  }
  plot_matrix
}

ensure_chapter5_figure_dir <- function(paths) {
  dir.create(paths$figures, recursive = TRUE, showWarnings = FALSE)
  paths$figures
}

render_plot <- function(plot_df, figure_dir) {
  figure_path <- file.path(figure_dir, "nam_coefficients.png")
  png(filename = figure_path, width = 1200, height = 600, res = 150)
  on.exit(dev.off(), add = TRUE)

  y_matrix <- cbind(plot_df$global, plot_df$peer)
  matplot(
    x = seq_len(nrow(plot_df)),
    y = y_matrix,
    type = "b",
    pch = 19,
    lty = 1,
    lwd = 2,
    col = c("#1f77b4", "#ff7f0e"),
    xaxt = "n",
    xlab = "Time period",
    ylab = "Coefficient estimate",
    main = "Chapter 5 NAM coefficient trends"
  )
  axis(1, at = seq_len(nrow(plot_df)), labels = plot_df$time_period)
  abline(h = 0, lty = 3, col = "#aaaaaa")
  legend(
    "topright",
    legend = c("Global misperception", "Peer misperception"),
    col = c("#1f77b4", "#ff7f0e"),
    lty = 1,
    lwd = 2,
    pch = 19,
    bg = "white"
  )

  figure_path
}

write_plot_data <- function(plot_df, paths) {
  plot_data_path <- file.path(paths$tables, "nam_plot_data.csv")
  write.csv(plot_df, plot_data_path, row.names = FALSE)
  plot_data_path
}

write_checksum_log <- function(bundle, tracked_paths, paths) {
  checksum_path <- file.path(paths$logs, "nam_checksums.json")
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  checksums <- tools::md5sum(tracked_paths)
  payload <- list(
    generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    files = lapply(names(checksums), function(path) {
      list(
        path = relative_repo_path(path, bundle$repo_root),
        md5 = unname(checksums[[path]])
      )
    })
  )
  write_json(payload, checksum_path, auto_unbox = TRUE, pretty = TRUE)
  checksum_path
}

main <- function() {
  bundle <- load_configuration()
  ensure_chapter_enabled(bundle, "chapter5_descriptive_norms")

  paths <- resolve_chapter5_outputs(bundle)
  summary_bundle <- load_summary(paths)
  plot_df <- prepare_plot_data(summary_bundle$data)

  plot_data_path <- write_plot_data(plot_df, paths)
  figure_dir <- ensure_chapter5_figure_dir(paths)
  figure_path <- render_plot(plot_df, figure_dir)

  related_paths <- c(summary_bundle$path, plot_data_path, figure_path)
  checksum_path <- write_checksum_log(bundle, related_paths, paths)

  message("[chapter5] Figure generated at ", figure_path)
  message("[chapter5] Plot data stored at ", plot_data_path)
  message("[chapter5] Checksum log written to ", checksum_path)
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error generating Chapter 5 figures: ", e$message)
      quit(status = 1)
    }
  )
}
