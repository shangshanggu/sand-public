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
  stop("Unable to determine script path for Chapter 5 validation.")
}

script_dir <- dirname(get_script_path())
repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

resolve_chapter5_outputs <- function(bundle) {
  resolve_chapter_output_paths(bundle, "chapter5_descriptive_norms")
}

detect_chapter4_synthetic <- function(bundle) {
  raw_dir <- resolve_data_dir(bundle)
  marker_files <- c(".chapter4_synthetic", ".realistic_proxy_data")
  for (marker in marker_files) {
    marker_path <- file.path(raw_dir, marker)
    if (file.exists(marker_path)) {
      return(list(
        present = TRUE,
        path = relative_repo_path(marker_path, bundle$repo_root)
      ))
    }
  }
  list(present = FALSE, path = NULL)
}

load_summary <- function(paths) {
  summary_path <- file.path(paths$tables, "nam_summary.csv")
  if (!file.exists(summary_path)) {
    stop(sprintf("Missing NAM summary at %s. Run 02_estimate_nam_models.R first.", summary_path))
  }
  list(data = read.csv(summary_path, stringsAsFactors = FALSE), path = summary_path)
}

load_expectations <- function(repo_root) {
  reference_path <- file.path(repo_root, "docs", "references", "quan_results.md")
  if (!file.exists(reference_path)) {
    stop("Expected reference document reproduced/docs/references/quan_results.md is missing.")
  }
  lines <- readLines(reference_path, warn = FALSE)
  content <- paste(lines, collapse = "\n")
  match <- regmatches(content, regexpr("```json[\\s\\S]*?```", content, perl = TRUE))
  if (length(match) == 0) {
    stop("Could not locate JSON expectations block in reproduced/docs/references/quan_results.md.")
  }
  json_text <- sub("^```json\\s*\\n", "", match)
  json_text <- sub("\\s*```$", "", json_text)
  parsed <- fromJSON(json_text, simplifyVector = FALSE)
  expectations <- parsed$chapter5_nam_expectations
  if (is.null(expectations)) {
    stop("JSON expectations block missing 'chapter5_nam_expectations' key.")
  }
  expectations
}

validate_coefficients <- function(summary_df, expectations) {
  # Support both old format (term column) and new lnam format (term_raw column)
  if ("term_raw" %in% names(summary_df) && !"term" %in% names(summary_df)) {
    # Map term_raw to expectation keys
    term_map <- c(
      "misperception_audit_c_global"   = "global_misperception",
      "misperception_audit_c_peer"     = "peer_misperception",
      "misperception_audit_score_peer" = "peer_misperception"
    )
    summary_df$term <- term_map[summary_df$term_raw]
  }

  keep <- summary_df$term %in% c("global_misperception", "peer_misperception")
  summary_filtered <- summary_df[keep, , drop = FALSE]
  results <- list()
  idx <- 1
  for (time_period in names(expectations)) {
    expected_terms <- expectations[[time_period]]
    for (term_name in names(expected_terms)) {
      expected <- expected_terms[[term_name]]
      actual_row <- summary_filtered[summary_filtered$time_period == time_period & summary_filtered$term == term_name, , drop = FALSE]
      if (nrow(actual_row) != 1) {
        stop(sprintf("Missing coefficient for %s / %s in NAM summary.", time_period, term_name))
      }
      estimate <- actual_row$estimate[[1]]
      tolerance <- if (!is.null(expected$tolerance)) expected$tolerance else 0.0
      difference <- abs(estimate - expected$estimate)
      passed <- difference <= tolerance
      results[[idx]] <- data.frame(
        time_period = time_period,
        term = term_name,
        expected_estimate = expected$estimate,
        actual_estimate = estimate,
        tolerance = tolerance,
        difference = difference,
        passed = passed,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  do.call(rbind, results)
}

write_validation_outputs <- function(validation_df, paths) {
  validation_csv <- file.path(paths$tables, "nam_validation.csv")
  validation_json <- file.path(paths$logs, "nam_validation.json")

  write.csv(validation_df, validation_csv, row.names = FALSE)
  write_json(list(results = validation_df), validation_json, auto_unbox = TRUE, pretty = TRUE)

  list(csv = validation_csv, json = validation_json)
}

main <- function() {
  bundle <- load_configuration()
  ensure_chapter_enabled(bundle, "chapter5_descriptive_norms")

  paths <- resolve_chapter5_outputs(bundle)
  summary_bundle <- load_summary(paths)
  expectations <- load_expectations(repo_root)

  validation_df <- validate_coefficients(summary_bundle$data, expectations)
  outputs <- write_validation_outputs(validation_df, paths)

  if (any(!validation_df$passed)) {
    synthetic_info <- detect_chapter4_synthetic(bundle)
    failing <- validation_df[!validation_df$passed, , drop = FALSE]
    if (isTRUE(synthetic_info$present)) {
      message(
        "[chapter5] Proxy-data benchmark warning: ", nrow(failing),
        " coefficient checks differ from protected-data targets (maximum absolute difference ",
        sprintf("%.4f", max(failing$difference, na.rm = TRUE)), "). Synthetic marker: ",
        synthetic_info$path,
        ". This is expected and is not an empirical reproduction claim."
      )
    } else {
      message("Validation failures detected:")
      print(failing, row.names = FALSE)
      stop("Chapter 5 NAM coefficients failed tolerance checks.")
    }
  } else {
    message("[chapter5] NAM coefficients validated successfully.")
  }
  invisible(outputs)
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error validating Chapter 5 NAM outputs: ", e$message)
      quit(status = 1)
    }
  )
}
