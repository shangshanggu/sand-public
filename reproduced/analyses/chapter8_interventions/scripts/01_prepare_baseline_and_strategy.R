#!/usr/bin/env Rscript
# 01_prepare_baseline_and_strategy.R
#
# Loads the converged SAOM fit from Chapter 7, extracts per-period parameter
# estimates, reads the strategy CSV, and expands the full intervention grid
# (type × wave × targeting × proportion × efficacy).
#
# Outputs:
#   - reproduced/outputs/chapter8/data/df_intervention_strategy.csv  (expanded grid)
#   - reproduced/outputs/chapter8/data/baseline_parameters.rds       (parameter table)
#   - reproduced/outputs/chapter8/logs/baseline_snapshot.json         (audit log)

suppressPackageStartupMessages({
library(yaml)
library(jsonlite)
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

# ---------------------------------------------------------------------------
# Extract SAOM parameter estimates per period from a sienaFit object
# Mirrors the original extract_estimates() from 0_baseline_setup.R
# ---------------------------------------------------------------------------
extract_estimates <- function(fit) {
  if (inherits(fit, "saomPlaceholder") || isTRUE(fit$placeholder)) {
    stop("Cannot run Chapter 8 interventions from a placeholder SAOM fit.\n",
         "Run Chapter 7 estimation first to produce a converged model.",
         call. = FALSE)
  }
  data.frame(
    effects   = fit$effects$effectName,
    estimate  = round(fit$theta, 3),
    std_error = round(sqrt(diag(fit$covtheta)), 3),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
run <- function() {
  cfg <- yaml::read_yaml(file.path(repo_root, "reproduced", "config", "thesis.yml"))
  int_cfg <- yaml::read_yaml(file.path(repo_root, "reproduced", "config",
                                        "scenarios", "interventions.yml"))
  logs_root <- file.path(repo_root, cfg$project$paths$logs_dir %||% "reproduced/logs")
  ch8_out  <- file.path(repo_root, cfg$chapters$chapter8_interventions$outputs_dir
                         %||% "reproduced/outputs/chapter8")

  # --- Load converged SAOM fit ---
  baseline_path <- file.path(repo_root,
    cfg$chapters$chapter7_saom$baseline_model$path %||%
    "reproduced/outputs/chapter7/cache/base_fit.RData")
  if (!file.exists(baseline_path)) {
    stop(sprintf("Baseline SAOM fit not found at %s.\n",
                 "Complete Chapter 7 estimation before running Chapter 8."),
         call. = FALSE)
  }
  env <- new.env(parent = emptyenv())
  load(baseline_path, envir = env)
  # Find the sienaFit object (original code stores it as ansGOF)
  fit <- NULL
  for (nm in ls(env, all.names = TRUE)) {
    obj <- get(nm, envir = env, inherits = FALSE)
    if (inherits(obj, "sienaFit")) { fit <- obj; break }
  }
  if (is.null(fit)) stop("No sienaFit object found in ", baseline_path, call. = FALSE)

  df_params <- extract_estimates(fit)
  # The original code replicates the same estimates across all periods
  # (the SAOM was estimated jointly; parameters are period-invariant for forward sim)
  df_base_parameters <- data.frame(
    effects = df_params$effects,
    oct_dec  = df_params$estimate,
    dec_mar  = df_params$estimate,
    mar_oct  = df_params$estimate,
    stringsAsFactors = FALSE
  )

  # --- Read strategy CSV and expand grid ---
  strategy_csv_rel <- int_cfg$strategy_csv %||%
    "config/scenarios/intervention_strategy_basecase.csv"
  strategy_csv <- file.path(repo_root, "reproduced", strategy_csv_rel)
  if (!file.exists(strategy_csv)) {
    strategy_csv <- file.path(dirname(file.path(repo_root, "reproduced", "config",
                                                 "scenarios", "interventions.yml")),
                               basename(strategy_csv_rel))
  }
  strategy_csv <- normalizePath(strategy_csv, winslash = "/", mustWork = TRUE)
  df_strategy_base <- read.csv(strategy_csv, stringsAsFactors = FALSE)

  defaults <- int_cfg$defaults %||% list()
  input_proportions <- as.integer(defaults$input_proportions %||% seq(0, 100, by = 10))
  input_efficacies  <- as.integer(defaults$input_efficacies  %||% c(75, 100, 125))

  # Expand: types A/B get proportion × efficacy; type C gets efficacy only
  rows_ab <- df_strategy_base[df_strategy_base$intervention_type %in% c("A", "B"), ]
  rows_c  <- df_strategy_base[df_strategy_base$intervention_type == "C", ]
  rows_other <- df_strategy_base[!df_strategy_base$intervention_type %in% c("A", "B", "C"), ]

  expanded_ab <- if (nrow(rows_ab) > 0) {
    do.call(rbind, lapply(seq_len(nrow(rows_ab)), function(i) {
      grid <- expand.grid(intervention_proportion = input_proportions,
                          intervention_efficacy   = input_efficacies,
                          stringsAsFactors = FALSE)
      cbind(rows_ab[rep(i, nrow(grid)), , drop = FALSE], grid, row.names = NULL)
    }))
  } else data.frame()

  expanded_c <- if (nrow(rows_c) > 0) {
    do.call(rbind, lapply(seq_len(nrow(rows_c)), function(i) {
      grid <- expand.grid(intervention_proportion = NA_integer_,
                          intervention_efficacy   = input_efficacies,
                          stringsAsFactors = FALSE)
      cbind(rows_c[rep(i, nrow(grid)), , drop = FALSE], grid, row.names = NULL)
    }))
  } else data.frame()

  df_strategy <- rbind(expanded_ab, expanded_c, rows_other)
  rownames(df_strategy) <- NULL

  # --- Write outputs ---
  ensure_parent_dir(file.path(ch8_out, "data", "df_intervention_strategy.csv"))
  write.csv(df_strategy, file.path(ch8_out, "data", "df_intervention_strategy.csv"),
            row.names = FALSE)

  saveRDS(df_base_parameters, file.path(ch8_out, "data", "baseline_parameters.rds"))

  snapshot <- list(
    timestamp       = format_timestamp(),
    baseline_path   = relative_repo_path(baseline_path, repo_root),
    strategy_csv    = relative_repo_path(strategy_csv, repo_root),
    n_base_rows     = nrow(df_strategy_base),
    n_expanded_rows = nrow(df_strategy),
    n_effects       = nrow(df_base_parameters),
    proportions     = input_proportions,
    efficacies      = input_efficacies,
    base_efficacy   = defaults$base_efficacy
  )
  ensure_parent_dir(file.path(ch8_out, "logs", "baseline_snapshot.json"))
  jsonlite::write_json(snapshot, file.path(ch8_out, "logs", "baseline_snapshot.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  entry <- list(timestamp = snapshot$timestamp, action = "prepare_baseline",
                scenarios = nrow(df_strategy))
  append_pipeline_log(logs_root, "chapter8", entry, history_key = "baseline_prep")

  message(sprintf("Baseline loaded (%d effects), strategy expanded to %d rows → %s",
                  nrow(df_base_parameters), nrow(df_strategy),
                  file.path(ch8_out, "data")))
}

run()
