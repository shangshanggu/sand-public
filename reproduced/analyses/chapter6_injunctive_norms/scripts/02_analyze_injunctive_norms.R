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
  stop("Unable to determine script path for Chapter 6 analysis.")
}

script_dir <- dirname(get_script_path())
repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

safe_mean <- function(x) {
  values <- as.numeric(x)
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    return(NA_real_)
  }
  mean(values)
}

safe_sd <- function(x) {
  values <- as.numeric(x)
  values <- values[!is.na(values)]
  if (length(values) < 2) {
    return(NA_real_)
  }
  stats::sd(values)
}

order_time_periods <- function(periods) {
  preferred <- c("Baseline", "Time 1", "Time 2", "Time 3", "Time 4", "Time 5")
  unique_periods <- unique(periods)
  ordered <- preferred[preferred %in% unique_periods]
  extras <- setdiff(unique_periods, ordered)
  c(ordered, sort(extras))
}

compute_summary <- function(data) {
  scenarios <- unique(data$scenario_key)
  results <- list()
  idx <- 1
  for (scenario in scenarios) {
    scenario_rows <- data[data$scenario_key == scenario, , drop = FALSE]
    if (nrow(scenario_rows) == 0) {
      next
    }
    periods <- order_time_periods(scenario_rows$time_period)
    for (period in periods) {
      subset_rows <- scenario_rows[scenario_rows$time_period == period, , drop = FALSE]
      if (nrow(subset_rows) == 0) {
        next
      }
      results[[idx]] <- data.frame(
        scenario_key = scenario,
        scenario_label = subset_rows$scenario_label[[1]],
        time_period = period,
        participants = length(unique(subset_rows$participant_id)),
        approval_mean = safe_mean(subset_rows$approval_value),
        approval_sd = safe_sd(subset_rows$approval_value),
        misperception_global_mean = safe_mean(subset_rows$misperception_global),
        misperception_peer_mean = safe_mean(subset_rows$misperception_peer),
        outcome_rate = safe_mean(subset_rows$outcome_value),
        global_gap = safe_mean(subset_rows$misperception_global) - safe_mean(subset_rows$approval_value),
        peer_gap = safe_mean(subset_rows$misperception_peer) - safe_mean(subset_rows$approval_value),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  if (length(results) == 0) {
    return(data.frame())
  }
  summary_df <- do.call(rbind, results)
  summary_df$time_period <- factor(summary_df$time_period, levels = order_time_periods(summary_df$time_period))
  summary_df[order(summary_df$scenario_key, summary_df$time_period), ]
}

compute_shift <- function(summary_df) {
  if (!nrow(summary_df)) {
    return(data.frame())
  }
  scenarios <- unique(summary_df$scenario_key)
  rows <- list()
  idx <- 1
  for (scenario in scenarios) {
    subset_rows <- summary_df[summary_df$scenario_key == scenario, , drop = FALSE]
    if (nrow(subset_rows) == 0) {
      next
    }
    ordered <- subset_rows[order(subset_rows$time_period), , drop = FALSE]
    label <- ordered$scenario_label[[1]]
    get_metric <- function(period, column) {
      value <- ordered[ordered$time_period == period, column]
      if (length(value) == 0) {
        return(NA_real_)
      }
      as.numeric(value[[1]])
    }
    baseline <- get_metric("Baseline", "approval_mean")
    time1 <- get_metric("Time 1", "approval_mean")
    time3 <- get_metric("Time 3", "approval_mean")
    global_baseline <- get_metric("Baseline", "global_gap")
    global_time3 <- get_metric("Time 3", "global_gap")
    peer_baseline <- get_metric("Baseline", "peer_gap")
    peer_time3 <- get_metric("Time 3", "peer_gap")
    outcome_baseline <- get_metric("Baseline", "outcome_rate")
    outcome_time3 <- get_metric("Time 3", "outcome_rate")

    rows[[idx]] <- data.frame(
      scenario_key = scenario,
      scenario_label = label,
      comparison = "Baseline→Time 3",
      approval_change = time3 - baseline,
      global_gap_change = global_time3 - global_baseline,
      peer_gap_change = peer_time3 - peer_baseline,
      outcome_rate_change = outcome_time3 - outcome_baseline,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1

    rows[[idx]] <- data.frame(
      scenario_key = scenario,
      scenario_label = label,
      comparison = "Time 1→Time 3",
      approval_change = time3 - time1,
      global_gap_change = global_time3 - get_metric("Time 1", "global_gap"),
      peer_gap_change = peer_time3 - get_metric("Time 1", "peer_gap"),
      outcome_rate_change = outcome_time3 - get_metric("Time 1", "outcome_rate"),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
  if (length(rows) == 0) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

write_summary_outputs <- function(summary_df, paths) {
  summary_csv <- file.path(paths$tables, "injunctive_summary.csv")
  summary_json <- file.path(paths$manifests, "injunctive_summary.json")
  write.csv(summary_df, summary_csv, row.names = FALSE)
  payload <- list(
    generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    rows = nrow(summary_df),
    data = summary_df
  )
  write_json(payload, summary_json, auto_unbox = TRUE, pretty = TRUE)
  list(csv = summary_csv, json = summary_json)
}

write_shift_outputs <- function(shift_df, paths) {
  shift_csv <- file.path(paths$tables, "innovation_shift_summary.csv")
  shift_json <- file.path(paths$manifests, "innovation_shift_summary.json")
  write.csv(shift_df, shift_csv, row.names = FALSE)
  payload <- list(
    generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    rows = nrow(shift_df),
    data = shift_df
  )
  write_json(payload, shift_json, auto_unbox = TRUE, pretty = TRUE)
  list(csv = shift_csv, json = shift_json)
}

write_dashboard_payload <- function(summary_df, paths) {
  dashboard_path <- file.path(paths$manifests, "injunctive_dashboard.json")
  scenario_summaries <- lapply(split(summary_df, summary_df$scenario_key), function(scenario_df) {
    ordered <- scenario_df[order(scenario_df$time_period), , drop = FALSE]
    list(
      key = ordered$scenario_key[[1]],
      label = ordered$scenario_label[[1]],
      time_series = lapply(seq_len(nrow(ordered)), function(i) {
        list(
          time_period = as.character(ordered$time_period[[i]]),
          approval_mean = ordered$approval_mean[[i]],
          misperception_global_mean = ordered$misperception_global_mean[[i]],
          misperception_peer_mean = ordered$misperception_peer_mean[[i]],
          outcome_rate = ordered$outcome_rate[[i]],
          global_gap = ordered$global_gap[[i]],
          peer_gap = ordered$peer_gap[[i]]
        )
      })
    )
  })
  payload <- list(
    generated_at = format_timestamp(),
    scenarios = scenario_summaries
  )
  write_json(payload, dashboard_path, auto_unbox = TRUE, pretty = TRUE)
  dashboard_path
}

copy_public_artifacts <- function(bundle, paths, summary_paths, shift_paths, dashboard_path) {
  public_dir <- file.path(paths$base, "public")
  dir.create(public_dir, recursive = TRUE, showWarnings = FALSE)
  public_summary <- file.path(public_dir, "injunctive_summary.csv")
  public_shift <- file.path(public_dir, "innovation_shift_summary.csv")
  public_dashboard <- file.path(public_dir, "injunctive_dashboard.json")
  file.copy(summary_paths$csv, public_summary, overwrite = TRUE)
  file.copy(shift_paths$csv, public_shift, overwrite = TRUE)
  file.copy(dashboard_path, public_dashboard, overwrite = TRUE)
  list(summary = public_summary, shift = public_shift, dashboard = public_dashboard)
}

update_run_log <- function(bundle, paths, input_path, outputs, public_outputs, logs_root) {
  log_path <- file.path(paths$logs, "run.json")
  entry <- list(
    generated_at = format_timestamp(),
    input = relative_repo_path(input_path, bundle$repo_root),
    outputs = relativise_paths(outputs, bundle$repo_root),
    public_exports = relativise_paths(public_outputs, bundle$repo_root)
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

  longitudinal_rel <- get_config_value(bundle, "chapters", "chapter6_injunctive_norms", "longitudinal_data", required = TRUE)
  longitudinal_path <- resolve_repo_path(bundle, longitudinal_rel, must_exist = TRUE)

  logs_rel <- get_config_value(bundle, "project", "paths", "logs_dir", required = TRUE)
  logs_root <- resolve_repo_path(bundle, logs_rel, must_exist = FALSE)

  longitudinal_data <- readRDS(longitudinal_path)
  if (!is.data.frame(longitudinal_data) || nrow(longitudinal_data) == 0) {
    stop("Longitudinal approval dataset is empty; rerun 01_prepare_approval_longitudinal.R.")
  }
  longitudinal_data$time_period <- as.character(longitudinal_data$time_period)

  summary_df <- compute_summary(longitudinal_data)
  if (!nrow(summary_df)) {
    stop("Unable to compute Chapter 6 summary statistics.")
  }

  shift_df <- compute_shift(summary_df)

  summary_paths <- write_summary_outputs(summary_df, paths)
  shift_paths <- write_shift_outputs(shift_df, paths)
  dashboard_path <- write_dashboard_payload(summary_df, paths)

  public_outputs <- copy_public_artifacts(bundle, paths, summary_paths, shift_paths, dashboard_path)

  outputs <- list(summary = summary_paths, shift = shift_paths, dashboard = dashboard_path)
  log_path <- update_run_log(bundle, paths, longitudinal_path, outputs, public_outputs, logs_root)

  message("[chapter6] Injunctive summary written to ", summary_paths$csv)
  message("[chapter6] Shift summary written to ", shift_paths$csv)
  message("[chapter6] Dashboard payload stored at ", dashboard_path)
  message("[chapter6] Public exports refreshed under ", dirname(public_outputs$summary))
  message("[chapter6] Run log updated at ", log_path)
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error analysing Chapter 6 injunctive norms: ", e$message)
      quit(status = 1)
    }
  )
}
