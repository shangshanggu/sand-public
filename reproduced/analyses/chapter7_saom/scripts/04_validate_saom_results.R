#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The 'yaml' package is required. Install it via install.packages('yaml').", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required. Install it via install.packages('jsonlite').", call. = FALSE)
  }
})

library(jsonlite)
library(yaml)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (is.character(x) && length(x) == 1 && !nzchar(x)) {
    return(y)
  }
  x
}

map_target_effect <- function(key) {
  default_map <- list(
    peer_influence = "average similarity",
    behaviour_linear_shape = "linear shape",
    alcohol_use_global = "linear shape"
  )
  default_map[[key]] %||% key
}

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- grep(file_arg, args, fixed = TRUE)
  if (length(matches) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", args[matches[1]]), winslash = "/", mustWork = TRUE)))
  }
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

parse_args <- function(default_model) {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- list(model = default_model, config = "config/thesis.yml")
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--model") {
      if (i == length(args)) stop("--model requires an identifier.")
      i <- i + 1
      opts$model <- args[[i]]
    } else if (arg == "--config") {
      if (i == length(args)) stop("--config requires a path.")
      i <- i + 1
      opts$config <- args[[i]]
    } else if (arg %in% c("--help", "-h")) {
      cat("Usage: Rscript 04_validate_saom_results.R [--model <id>] [--config <path>]\n")
      quit(status = 0)
    } else {
      stop(sprintf("Unrecognised argument '%s'", arg))
    }
    i <- i + 1
  }
  opts
}

relative_or_absolute <- function(path, repo_root) {
  if (is.null(path) || !nzchar(path)) {
    return(NULL)
  }
  if (exists("relative_repo_path", mode = "function")) {
    return(relative_repo_path(path, repo_root))
  }
  path
}

load_coefficients <- function(table_path) {
  if (!file.exists(table_path)) {
    return(NULL)
  }
  tryCatch({
    utils::read.csv(table_path, stringsAsFactors = FALSE)
  }, error = function(err) {
    warning(sprintf("Failed to read coefficient table at %s: %s", table_path, conditionMessage(err)))
    NULL
  })
}

extract_metric_entry <- function(metric, targets, coeff_df, tolerance, run_log) {
  target_value <- targets[[metric]]
  effect_key <- map_target_effect(metric)
  estimate <- NA_real_
  effect_name <- effect_key
  source <- "coefficients_table"

  if (!is.null(run_log$target_diagnostics)) {
    matches <- Filter(function(entry) identical(entry$metric, metric), run_log$target_diagnostics)
    if (length(matches)) {
      entry <- matches[[1]]
      estimate <- entry$estimate %||% NA_real_
      effect_name <- entry$effect %||% effect_key
      source <- "run_log"
    }
  }

  if (is.na(estimate) && !is.null(coeff_df) && nrow(coeff_df)) {
    if ("effect" %in% names(coeff_df)) {
      idx <- which(coeff_df$effect == effect_key)
      if (!length(idx) && "label" %in% names(coeff_df)) {
        idx <- which(coeff_df$label == effect_key)
      }
      if (!length(idx) && "label" %in% names(coeff_df)) {
        idx <- which(grepl(effect_key, coeff_df$label, fixed = TRUE))
      }
      if (length(idx)) {
        estimate <- coeff_df$estimate[idx[1]]
        effect_name <- coeff_df$label[idx[1]] %||% coeff_df$effect[idx[1]]
        source <- "coefficients_table"
      }
    }
  }

  delta <- if (is.na(estimate) || is.na(target_value)) NA_real_ else estimate - target_value
  within <- if (is.na(delta) || is.na(tolerance)) NA else abs(delta) <= tolerance

  list(
    metric = metric,
    effect = effect_name,
    target = target_value,
    estimate = if (is.na(estimate)) NULL else estimate,
    delta = if (is.na(delta)) NULL else delta,
    within_tolerance = if (is.na(within)) NULL else within,
    tolerance = tolerance,
    source = source
  )
}

main <- function() {
  script_dir <- get_script_dir()
  repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
  setwd(repo_root)

  source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
  source(file.path(repo_root, "R", "common.R"))

  config_bundle <- load_configuration(file.path(repo_root, "config", "thesis.yml"))
  chapter_cfg <- config_bundle$config$chapters$chapter7_saom
  if (is.null(chapter_cfg)) {
    stop("chapter7_saom configuration block missing from thesis.yml")
  }

  args <- parse_args(chapter_cfg$default_model %||% names(chapter_cfg$target_coefficients)[1] %||% "base")
  if (!identical(normalizePath(args$config, winslash = "/", mustWork = FALSE), config_bundle$config_path)) {
    config_bundle <- load_configuration(args$config)
    chapter_cfg <- config_bundle$config$chapters$chapter7_saom
    if (is.null(chapter_cfg)) {
      stop("chapter7_saom configuration block missing from provided configuration")
    }
  }

  ensure_chapter_enabled(config_bundle, "chapter7_saom")

  model_id <- args$model
  output_paths <- resolve_chapter_output_paths(config_bundle, "chapter7_saom", create = TRUE)
  logs_dir <- file.path(output_paths$base, "logs")
  validations_dir <- file.path(output_paths$base, "validations")
  dir.create(validations_dir, recursive = TRUE, showWarnings = FALSE)

  run_log_path <- file.path(logs_dir, sprintf("saom_run_%s.json", model_id))
  if (!file.exists(run_log_path)) {
    stop(sprintf("SAOM run log not found at %s. Run 02_run_saom_model.R before validation.", run_log_path))
  }

  run_log <- jsonlite::fromJSON(run_log_path, simplifyVector = FALSE)
  coefficient_table <- file.path(output_paths$base, "tables", sprintf("saom_coefficients_%s.csv", model_id))
  coeff_df <- load_coefficients(coefficient_table)

  targets <- chapter_cfg$target_coefficients %||% list()
  if (!length(targets)) {
    warning("No target coefficients configured; nothing to validate.")
    return(invisible(TRUE))
  }

  tolerance <- chapter_cfg$target_tolerance %||% config_bundle$config$rsiena$diagnostics$target_tolerance %||% 0.1
  generated_at <- format(Sys.time(), tz = config_bundle$config$project$timezone %||% "UTC")

  checks <- lapply(names(targets), function(metric) {
    extract_metric_entry(metric, targets, coeff_df, tolerance, run_log)
  })

  within <- vapply(checks, function(entry) isTRUE(entry$within_tolerance), logical(1))
  has_failures <- length(within) && !all(within)
  placeholder <- isTRUE(run_log$placeholder)
  status <- if (placeholder) "placeholder" else if (has_failures) "failed" else "passed"

  validation <- list(
    model = model_id,
    generated_at = generated_at,
    status = status,
    placeholder = if (placeholder) TRUE else FALSE,
    tolerance = tolerance,
    checks = checks,
    sources = list(
      run_log = relative_or_absolute(run_log_path, config_bundle$repo_root),
      coefficients_table = if (!is.null(coeff_df)) relative_or_absolute(coefficient_table, config_bundle$repo_root) else NULL
    )
  )

  log_path <- file.path(logs_dir, sprintf("saom_coefficient_validation_%s.json", model_id))
  validation_path <- file.path(validations_dir, sprintf("saom_coefficient_validation_%s.json", model_id))
  jsonlite::write_json(validation, log_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  jsonlite::write_json(validation, validation_path, pretty = TRUE, auto_unbox = TRUE, na = "null")

  if (status == "passed") {
    message(sprintf("SAOM coefficient validation passed for model '%s'.", model_id))
  } else if (status == "placeholder") {
    message(sprintf("SAOM coefficient validation recorded placeholder outputs for model '%s'.", model_id))
  } else {
    message(sprintf("SAOM coefficient validation found discrepancies for model '%s'.", model_id))
  }

  invisible(TRUE)
}

if (identical(environment(), globalenv())) {
  tryCatch(main(), error = function(err) {
    message("Error validating SAOM coefficients: ", conditionMessage(err))
    quit(status = 1)
  })
}
