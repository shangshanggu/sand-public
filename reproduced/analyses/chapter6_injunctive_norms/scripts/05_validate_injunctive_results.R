#!/usr/bin/env Rscript
#
# 05_validate_injunctive_results.R
#
# Compare reproduced Chapter 6 NAM coefficients against thesis expectations
# stored in reproduced/docs/references/quan_results.md.  Follows the same
# pattern as Chapter 5's 05_compare_nam_results.R.

suppressPackageStartupMessages({
  library(jsonlite)
})

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- grep(file_arg, cmd_args)
  if (length(matches) > 0) {
    return(normalizePath(sub(file_arg, "", cmd_args[matches[1]]),
                         winslash = "/", mustWork = TRUE))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile,
                         winslash = "/", mustWork = TRUE))
  }
  stop("Unable to determine script path for Chapter 6 validation.")
}

script_dir <- dirname(get_script_path())
repo_root  <- normalizePath(file.path(script_dir, "..", "..", ".."),
                            winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

# ---------------------------------------------------------------------------
# Load current coefficients
# ---------------------------------------------------------------------------
load_current <- function(paths) {
  csv_path <- file.path(paths$tables, "injunctive_nam_coefficients.csv")
  if (!file.exists(csv_path)) {
    stop(sprintf("Missing %s. Run 04_estimate_injunctive_nam_models.R first.",
                 csv_path))
  }
  list(data = read.csv(csv_path, stringsAsFactors = FALSE), path = csv_path)
}

# ---------------------------------------------------------------------------
# Load thesis expectations from quan_results.md
# ---------------------------------------------------------------------------
load_expectations <- function(repo_root) {
  ref_path <- file.path(repo_root, "docs", "references", "quan_results.md")
  if (!file.exists(ref_path)) {
    stop("Missing reproduced/docs/references/quan_results.md")
  }
  lines <- readLines(ref_path, warn = FALSE)
  content <- paste(lines, collapse = "\n")

  # Find the Chapter 6 JSON block
  all_blocks <- gregexpr("```json[\\s\\S]*?```", content, perl = TRUE)
  matches <- regmatches(content, all_blocks)[[1]]

  ch6_block <- NULL
  for (m in matches) {
    if (grepl("chapter6_injunctive_expectations", m, fixed = TRUE)) {
      ch6_block <- m
      break
    }
  }
  if (is.null(ch6_block)) {
    stop("Could not find chapter6_injunctive_expectations JSON block in quan_results.md")
  }

  json_text <- sub("^```json\\s*\\n", "", ch6_block)
  json_text <- sub("\\s*```$", "", json_text)
  parsed <- fromJSON(json_text, simplifyVector = FALSE)
  expectations <- parsed$chapter6_injunctive_expectations
  if (is.null(expectations)) {
    stop("JSON block missing 'chapter6_injunctive_expectations' key.")
  }
  expectations
}


# ---------------------------------------------------------------------------
# Build comparison data frame
# ---------------------------------------------------------------------------

# Map from term_raw in the coefficients CSV to the keys used in expectations
TERM_MAP <- c(
  "audit_score_previous" = "audit_score_previous",
  "age"                  = "age",
  "sex"                  = "sex",
  "if_white"             = "if_white",
  "friend_number"        = "friend_number"
)

build_comparison <- function(current_df, expectations) {
  rows <- list()
  idx <- 1

  for (outcome_key in names(expectations)) {
    outcome_exp <- expectations[[outcome_key]]
    for (period in names(outcome_exp)) {
      period_exp <- outcome_exp[[period]]
      for (term_key in names(period_exp)) {
        exp <- period_exp[[term_key]]

        # Find matching row in current coefficients
        # term_raw for misperception columns varies by outcome, so match on
        # term_key against term_raw or the TERM_MAP
        current_rows <- current_df[
          current_df$outcome_key == outcome_key &
          current_df$time_period == period, , drop = FALSE]

        # Match: misperception terms use the term_raw that contains the key
        matched <- NULL
        if (term_key == "global_misperception") {
          matched <- current_rows[grepl("misperception.*global", current_rows$term_raw), ]
        } else if (term_key == "peer_misperception") {
          matched <- current_rows[grepl("misperception.*peer", current_rows$term_raw), ]
        } else {
          matched <- current_rows[current_rows$term_raw == term_key, ]
        }

        current_est <- if (!is.null(matched) && nrow(matched) == 1) {
          matched$estimate
        } else {
          NA_real_
        }
        current_se <- if (!is.null(matched) && nrow(matched) == 1) {
          matched$std_error
        } else {
          NA_real_
        }

        expected_est <- exp$estimate
        tolerance    <- if (!is.null(exp$tolerance)) exp$tolerance else 0.02

        diff <- current_est - expected_est
        within_tol <- !is.na(diff) && abs(diff) <= tolerance

        rows[[idx]] <- data.frame(
          outcome_key       = outcome_key,
          time_period       = period,
          term              = term_key,
          current_estimate  = current_est,
          current_std_error = current_se,
          expected_estimate = expected_est,
          expected_std_error = if (!is.null(exp$std_error)) exp$std_error else NA_real_,
          tolerance         = tolerance,
          difference        = diff,
          abs_difference    = abs(diff),
          within_tolerance  = within_tol,
          stringsAsFactors  = FALSE
        )
        idx <- idx + 1
      }
    }
  }

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# Detect synthetic data
# ---------------------------------------------------------------------------
detect_synthetic <- function(bundle) {
  raw_dir <- resolve_data_dir(bundle)
  markers <- c(".chapter4_synthetic", ".realistic_proxy_data")
  for (m in markers) {
    p <- file.path(raw_dir, m)
    if (file.exists(p)) {
      return(list(present = TRUE, path = relative_repo_path(p, bundle$repo_root)))
    }
  }
  list(present = FALSE, path = NULL)
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
main <- function() {
  bundle <- load_configuration()
  ensure_chapter_enabled(bundle, "chapter6_injunctive_norms")

  paths <- resolve_chapter_output_paths(bundle, "chapter6_injunctive_norms")
  current <- load_current(paths)
  expectations <- load_expectations(repo_root)
  synthetic <- detect_synthetic(bundle)

  comparison <- build_comparison(current$data, expectations)

  # Write outputs
  csv_path  <- file.path(paths$tables, "injunctive_nam_comparison.csv")
  json_path <- file.path(paths$manifests, "injunctive_nam_validation.json")

  write.csv(comparison, csv_path, row.names = FALSE)

  n_total   <- nrow(comparison)
  n_pass    <- sum(comparison$within_tolerance, na.rm = TRUE)
  n_fail    <- sum(!comparison$within_tolerance, na.rm = TRUE)
  n_missing <- sum(is.na(comparison$within_tolerance))
  all_pass  <- n_fail == 0 && n_missing == 0

  status <- if (all_pass) "passed" else if (isTRUE(synthetic$present)) "warning" else "failed"

  payload <- list(
    generated_at    = format_timestamp(),
    status          = status,
    estimation_note = "sna::lnam() network autocorrelation model, matching thesis methodology.",
    total_checks    = n_total,
    passed          = n_pass,
    failed          = n_fail,
    missing         = n_missing,
    max_abs_diff    = if (n_total > 0) max(comparison$abs_difference, na.rm = TRUE) else NA_real_,
    synthetic_data  = synthetic$present,
    comparison      = comparison
  )
  write_json(payload, json_path, auto_unbox = TRUE, pretty = TRUE)

  # Report
  if (!all_pass) {
    failing <- comparison[!comparison$within_tolerance | is.na(comparison$within_tolerance), ]
    if (isTRUE(synthetic$present)) {
      message(
        "[chapter6] Proxy-data benchmark warning: ", nrow(failing),
        " coefficient checks differ from protected-data targets (maximum absolute difference ",
        sprintf("%.4f", max(failing$abs_difference, na.rm = TRUE)), "). Synthetic marker: ",
        synthetic$path,
        ". This is expected and is not an empirical reproduction claim."
      )
    } else {
      message("Chapter 6 NAM validation mismatches:")
      print(failing[, c("outcome_key", "time_period", "term",
                         "current_estimate", "expected_estimate",
                         "difference", "tolerance")], row.names = FALSE)
      stop("Chapter 6 NAM coefficients diverged beyond tolerance.")
    }
  } else {
    message("[chapter6] NAM coefficient validation passed. ",
            n_pass, "/", n_total, " checks within tolerance.")
  }

  # Update run log
  logs_rel  <- get_config_value(bundle, "project", "paths", "logs_dir",
                                required = TRUE)
  logs_root <- resolve_repo_path(bundle, logs_rel, must_exist = FALSE)
  log_entry <- list(
    generated_at = format_timestamp(),
    validation_status = status,
    outputs = relativise_paths(
      list(comparison_csv = csv_path, validation_json = json_path),
      bundle$repo_root
    )
  )
  log_path <- file.path(paths$logs, "run.json")
  append_run_log(log_path, log_entry)
  if (!is.null(logs_root) && nzchar(logs_root)) {
    append_pipeline_log(logs_root, "chapter6", log_entry)
  }

  message("[chapter6] Comparison written to ", csv_path)
  message("[chapter6] Validation manifest stored at ", json_path)
  message("[chapter6] Run log updated at ", log_path)
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error validating Chapter 6 results: ", e$message)
      quit(status = 1)
    }
  )
}
