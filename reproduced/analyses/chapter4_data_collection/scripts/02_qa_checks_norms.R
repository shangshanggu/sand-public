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
  stop("Unable to determine script path for configuration loader.")
}

script_dir <- dirname(get_script_path())
repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

usage <- function() {
  message(
    "Usage: Rscript reproduced/analyses/chapter4_data_collection/scripts/02_qa_checks_norms.R [options]\n",
    "\n",
    "Options:\n",
    "  --config <path>         Path to thesis configuration (default: config/thesis.yml)\n",
    "  --qa-report <path>      Path to chapter4_qa_report.json (defaults from config)\n",
    "  --output <path>         Where to write chapter4_qa_assertions.json (defaults from config)\n",
    "  --allow-degraded        Exit successfully even if expectations fail (records failure in output)\n",
    "  --help                  Display this message\n"
  )
}

parse_args <- function(args) {
  opts <- list(
    config = "config/thesis.yml",
    qa_report = NULL,
    output = NULL,
    allow_degraded = FALSE
  )
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--config") {
      if (i == length(args)) stop("--config requires a path argument.")
      i <- i + 1
      opts$config <- args[[i]]
    } else if (arg == "--qa-report") {
      if (i == length(args)) stop("--qa-report requires a path argument.")
      i <- i + 1
      opts$qa_report <- args[[i]]
    } else if (arg == "--output") {
      if (i == length(args)) stop("--output requires a path argument.")
      i <- i + 1
      opts$output <- args[[i]]
    } else if (arg == "--allow-degraded") {
      opts$allow_degraded <- TRUE
    } else if (arg %in% c("--help", "-h")) {
      usage()
      quit(status = 0)
    } else {
      stop(sprintf("Unknown argument: %s", arg))
    }
    i <- i + 1
  }
  opts
}

load_qa_report <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("QA report not found at %s. Run 01_data_preparation_norms.R first.", path))
  }
  jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
}

add_assertion <- function(assertions, id, description, expected, actual, passed, metadata = list()) {
  assertion <- list(
    id = id,
    description = description,
    expected = expected,
    actual = actual,
    passed = isTRUE(passed),
    metadata = metadata
  )
  append(assertions, list(assertion))
}

detect_synthetic_marker <- function(config_bundle) {
  raw_dir <- resolve_data_dir(config_bundle)
  marker_files <- c(".chapter4_synthetic", ".realistic_proxy_data")
  for (marker in marker_files) {
    marker_path <- file.path(raw_dir, marker)
    if (file.exists(marker_path)) {
      return(list(
        present = TRUE,
        path = relative_repo_path(marker_path, config_bundle$repo_root)
      ))
    }
  }
  list(present = FALSE, path = NULL)
}

build_assertions <- function(qa_data, expectations) {
  waves <- qa_data$waves
  if (is.null(waves) || !is.data.frame(waves) || nrow(waves) == 0) {
    stop("QA report does not include wave-level participant metrics.")
  }
  if (!"wave_index" %in% names(waves)) {
    waves$wave_index <- seq_len(nrow(waves))
  }
  order_indices <- waves$wave_index
  order_indices[is.na(order_indices)] <- seq_along(order_indices)[is.na(order_indices)]
  waves <- waves[order(order_indices, seq_len(nrow(waves))), , drop = FALSE]

  min_participants <- expectations$min_participants_per_wave %||% stop("Missing min_participants_per_wave in configuration.")
  tolerance <- expectations$coverage_tolerance %||% stop("Missing coverage_tolerance in configuration.")

  baseline <- waves$participants[1]
  if (is.null(baseline) || is.na(baseline)) {
    stop("Baseline participant count missing from QA report.")
  }

  assertions <- list()
  for (idx in seq_len(nrow(waves))) {
    wave <- waves[idx, ]
    wave_label <- wave$wave_label %||% sprintf("wave_%s", wave$wave_index %||% idx)
    participants <- wave$participants %||% NA
    metadata <- list(
      wave_index = wave$wave_index %||% idx,
      wave_label = wave_label
    )

    meets_minimum <- !is.na(participants) && participants >= min_participants
    assertions <- add_assertion(
      assertions,
      sprintf("wave_%s_minimum", metadata$wave_index),
      sprintf("Wave %s participant count meets minimum", wave_label),
      list(minimum_participants = min_participants),
      list(participants = participants),
      meets_minimum,
      metadata
    )

    if (idx == 1) {
      relative_drop <- 0
      within_tolerance <- TRUE
    } else if (baseline <= 0 || is.na(baseline)) {
      relative_drop <- NA
      within_tolerance <- FALSE
    } else {
      relative_drop <- max(0, 1 - (participants %||% 0) / baseline)
      within_tolerance <- !is.na(relative_drop) && relative_drop <= tolerance
    }

    assertions <- add_assertion(
      assertions,
      sprintf("wave_%s_retention", metadata$wave_index),
      sprintf("Wave %s retention within %.2f tolerance", wave_label, tolerance),
      list(max_relative_drop = tolerance),
      list(relative_drop = relative_drop),
      within_tolerance,
      metadata
    )
  }

  list(
    baseline = baseline,
    assertions = assertions,
    overall_pass = all(vapply(assertions, function(x) isTRUE(x$passed), logical(1)))
  )
}

print_summary <- function(result, expectations, allow_degraded = FALSE, synthetic_info = list(present = FALSE, path = NULL)) {
  cat(sprintf(
    "Chapter 4 QA coverage summary (baseline participants: %s; minimum required per wave: %s; proxy_tolerance: %s; synthetic_data: %s)\n",
    result$baseline,
    expectations$min_participants_per_wave,
    allow_degraded,
    if (isTRUE(synthetic_info$present)) "yes" else "no"
  ))
  if (isTRUE(synthetic_info$present) && !is.null(synthetic_info$path)) {
    cat(sprintf("Synthetic marker detected at %s\n", synthetic_info$path))
  }
  for (assertion in result$assertions) {
    status <- if (isTRUE(assertion$passed)) "PASS" else "FAIL"
    detail <- if (!is.null(assertion$actual$participants)) {
      sprintf("participants=%s", assertion$actual$participants)
    } else if (!is.null(assertion$actual$relative_drop)) {
      sprintf("relative_drop=%.3f", assertion$actual$relative_drop)
    } else {
      ""
    }
    cat(sprintf("[%s] %s %s\n", status, assertion$description, detail))
  }
  invisible(TRUE)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- parse_args(args)

  config_path <- resolve_existing_path(opts$config)
  config_bundle <- load_configuration(config_path)
  opts$config <- config_bundle$config_path
  ensure_chapter_enabled(config_bundle, "chapter4_data_collection")

  chapter_paths <- resolve_chapter_output_paths(config_bundle, "chapter4_data_collection")

  if (is.null(opts$qa_report)) {
    qa_report_path <- file.path(chapter_paths$logs, "chapter4_qa_report.json")
    qa_report_relative <- relative_repo_path(qa_report_path, config_bundle$repo_root)
    if (!file.exists(qa_report_path)) {
      stop(sprintf("QA report not found at %s. Run 01_data_preparation_norms.R first.", qa_report_path))
    }
  } else {
    qa_report_path <- normalizePath(opts$qa_report, winslash = "/", mustWork = TRUE)
    qa_report_relative <- relative_repo_path(qa_report_path, config_bundle$repo_root)
  }

  if (is.null(opts$output)) {
    output_path <- file.path(chapter_paths$logs, "chapter4_qa_assertions.json")
    output_relative <- relative_repo_path(output_path, config_bundle$repo_root)
  } else {
    output_path <- normalizePath(opts$output, winslash = "/", mustWork = FALSE)
    output_relative <- relative_repo_path(output_path, config_bundle$repo_root)
  }

  qa_data <- load_qa_report(qa_report_path)
  expectations <- get_config_value(config_bundle, "chapters", "chapter4_data_collection", "qa_expectations", required = TRUE)
  result <- build_assertions(qa_data, expectations)
  synthetic_info <- detect_synthetic_marker(config_bundle)
  print_summary(result, expectations, opts$allow_degraded, synthetic_info)

  timezone <- get_config_value(config_bundle, "project", "timezone", default = "UTC")
  generated_at <- format(Sys.time(), tz = timezone, usetz = TRUE)

  output_payload <- list(
    generated_at = generated_at,
    config = relative_repo_path(opts$config, config_bundle$repo_root),
    qa_report = qa_report_relative,
    output = output_relative,
    allow_degraded = opts$allow_degraded,
    expectations = list(
      min_participants_per_wave = expectations$min_participants_per_wave,
      coverage_tolerance = expectations$coverage_tolerance
    ),
    synthetic_marker_present = synthetic_info$present,
    synthetic_marker_path = synthetic_info$path,
    baseline_participants = result$baseline,
    overall_pass = result$overall_pass,
    assertions = result$assertions
  )

  write_json(output_payload, output_path, pretty = TRUE, auto_unbox = TRUE)

  if (!isTRUE(result$overall_pass)) {
    message("Chapter 4 coverage expectations not met. See chapter4_qa_assertions.json for details.")
    if (!isTRUE(opts$allow_degraded)) {
      stop("Coverage checks failed.")
    }
  }

  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !length(sys.frames())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error: ", e$message)
      quit(status = 1)
    }
  )
}
