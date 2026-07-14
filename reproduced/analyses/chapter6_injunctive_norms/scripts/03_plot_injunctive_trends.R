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
  stop("Unable to determine script path for Chapter 6 plotting.")
}

script_dir <- dirname(get_script_path())
repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

load_summary_data <- function(paths) {
  summary_path <- file.path(paths$tables, "injunctive_summary.csv")
  if (!file.exists(summary_path)) {
    stop(sprintf("Missing injunctive summary at %s. Run 02_analyze_injunctive_norms.R first.", summary_path))
  }
  summary_df <- read.csv(summary_path, stringsAsFactors = FALSE)
  if (!nrow(summary_df)) {
    stop("Injunctive summary is empty; cannot generate plots.")
  }
  summary_df$time_period <- as.character(summary_df$time_period)
  list(data = summary_df, path = summary_path)
}

ensure_public_dir <- function(paths) {
  public_dir <- file.path(paths$base, "public")
  dir.create(public_dir, recursive = TRUE, showWarnings = FALSE)
  public_dir
}

plot_trends <- function(summary_df, figure_path) {
  scenarios <- unique(summary_df$scenario_key)
  if (length(scenarios) == 0) {
    stop("No scenarios available for plotting.")
  }
  periods <- c("Baseline", "Time 1", "Time 2", "Time 3", "Time 4", "Time 5")
  png(filename = figure_path, width = 1400, height = max(600, 400 * length(scenarios)), res = 150)
  on.exit(dev.off(), add = TRUE)
  par(mfrow = c(length(scenarios), 1), mar = c(4, 5, 3, 2))
  for (scenario in scenarios) {
    subset_rows <- summary_df[summary_df$scenario_key == scenario, , drop = FALSE]
    ordered <- subset_rows[match(periods[periods %in% subset_rows$time_period], subset_rows$time_period), , drop = FALSE]
    ordered <- ordered[!is.na(ordered$time_period), , drop = FALSE]
    if (nrow(ordered) == 0) {
      next
    }
    x_vals <- seq_len(nrow(ordered))
    x_labels <- ordered$time_period
    approval <- as.numeric(ordered$approval_mean)
    global <- as.numeric(ordered$misperception_global_mean)
    peer <- as.numeric(ordered$misperception_peer_mean)

    y_min <- min(c(approval, global, peer), na.rm = TRUE)
    y_max <- max(c(approval, global, peer), na.rm = TRUE)
    if (!is.finite(y_min) || !is.finite(y_max)) {
      y_min <- 0
      y_max <- 1
    }
    buffer <- (y_max - y_min) * 0.1
    if (!is.finite(buffer)) {
      buffer <- 0.1
    }

    plot(
      x = x_vals,
      y = approval,
      type = "b",
      pch = 19,
      lty = 1,
      lwd = 2,
      col = "#2c7fb8",
      xaxt = "n",
      ylim = c(y_min - buffer, y_max + buffer),
      xlab = "Time period",
      ylab = "Approval / misperception",
      main = ordered$scenario_label[[1]]
    )
    axis(1, at = x_vals, labels = x_labels)
    if (any(!is.na(global))) {
      lines(x_vals, global, type = "b", pch = 17, lty = 2, lwd = 2, col = "#fdae61")
    }
    if (any(!is.na(peer))) {
      lines(x_vals, peer, type = "b", pch = 15, lty = 3, lwd = 2, col = "#7fc97f")
    }
    legend(
      "topright",
      legend = c("Self approval", "Global misperception", "Peer misperception"),
      col = c("#2c7fb8", "#fdae61", "#7fc97f"),
      lty = c(1, 2, 3),
      pch = c(19, 17, 15),
      bty = "n",
      cex = 0.85
    )
    abline(h = 0, col = "#cccccc", lty = 3)
  }
  figure_path
}

write_checksum_log <- function(bundle, tracked_paths, paths) {
  checksum_path <- file.path(paths$logs, "chapter6_checksums.json")
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  tracked <- unique(Filter(
    function(path) !is.null(path) && file.exists(path),
    tracked_paths
  ))
  if (!length(tracked)) {
    warning("No files available for checksum logging.")
    return(checksum_path)
  }
  checksums <- tools::md5sum(tracked)
  payload <- list(
    generated_at = format_timestamp(),
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

update_run_log <- function(bundle, paths, summary_path, figure_path, checksum_path, logs_root) {
  log_path <- file.path(paths$logs, "run.json")
  entry <- list(
    generated_at = format_timestamp(),
    input = relative_repo_path(summary_path, bundle$repo_root),
    outputs = relativise_paths(
      list(
        trends_figure = figure_path,
        checksums = checksum_path
      ),
      bundle$repo_root
    )
  )
  append_run_log(log_path, entry)
  if (!is.null(logs_root) && nzchar(logs_root)) {
    append_pipeline_log(logs_root, "chapter6", entry)
  }
  log_path
}

main <- function() {
  bundle <- load_configuration()
  ensure_chapter_enabled(bundle, "chapter6_injunctive_norms")

  paths <- resolve_chapter_output_paths(bundle, "chapter6_injunctive_norms")

  summary_bundle <- load_summary_data(paths)
  summary_df <- summary_bundle$data

  figure_path <- file.path(paths$figures, "injunctive_trends.png")
  public_dir <- ensure_public_dir(paths)
  plot_trends(summary_df, figure_path)

  longitudinal_rel <- get_config_value(bundle, "chapters", "chapter6_injunctive_norms", "longitudinal_data", required = TRUE)
  longitudinal_rds <- resolve_repo_path(bundle, longitudinal_rel, must_exist = TRUE)
  longitudinal_csv <- if (grepl("\\.rds$", longitudinal_rds, ignore.case = TRUE)) {
    sub("\\.rds$", ".csv", longitudinal_rds, ignore.case = TRUE)
  } else {
    paste0(longitudinal_rds, ".csv")
  }
  metadata_path <- file.path(paths$manifests, "approval_longitudinal_metadata.json")

  shift_csv <- file.path(paths$tables, "innovation_shift_summary.csv")
  shift_json <- file.path(paths$manifests, "innovation_shift_summary.json")
  summary_json <- file.path(paths$manifests, "injunctive_summary.json")
  dashboard_json <- file.path(paths$manifests, "injunctive_dashboard.json")
  public_summary <- file.path(public_dir, "injunctive_summary.csv")
  public_shift <- file.path(public_dir, "innovation_shift_summary.csv")
  public_dashboard <- file.path(public_dir, "injunctive_dashboard.json")

  tracked_paths <- c(
    summary_bundle$path,
    summary_json,
    shift_csv,
    shift_json,
    dashboard_json,
    longitudinal_rds,
    if (file.exists(longitudinal_csv)) longitudinal_csv,
    metadata_path,
    figure_path,
    public_summary,
    public_shift,
    public_dashboard
  )

  checksum_path <- write_checksum_log(bundle, tracked_paths, paths)

  logs_rel <- get_config_value(bundle, "project", "paths", "logs_dir", required = TRUE)
  logs_root <- resolve_repo_path(bundle, logs_rel, must_exist = FALSE)

  log_path <- update_run_log(bundle, paths, summary_bundle$path, figure_path, checksum_path, logs_root)

  message("[chapter6] Injunctive trends figure generated at ", figure_path)
  message("[chapter6] Checksum log updated at ", checksum_path)
  message("[chapter6] Public artifacts staged under ", public_dir)
  message("[chapter6] Run log updated at ", log_path)
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error plotting Chapter 6 trends: ", e$message)
      quit(status = 1)
    }
  )
}
