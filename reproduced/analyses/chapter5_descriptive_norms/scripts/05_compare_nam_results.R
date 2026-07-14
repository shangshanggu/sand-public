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
  stop("Unable to determine script path for Chapter 5 comparison.")
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

load_current_summary <- function(paths) {
  summary_path <- file.path(paths$tables, "nam_summary.csv")
  if (!file.exists(summary_path)) {
    stop(sprintf("Missing NAM summary at %s. Run 02_estimate_nam_models.R first.", summary_path))
  }
  list(
    data = read.csv(summary_path, stringsAsFactors = FALSE),
    path = summary_path
  )
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

extract_current_coefficients <- function(summary_df) {
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
  filtered <- summary_df[keep, , drop = FALSE]

  # Select columns that exist (lnam format has different column names)
  out_cols <- intersect(c("time_period", "term", "estimate", "std_error", "t_value", "p_value"), names(filtered))
  filtered <- filtered[, out_cols, drop = FALSE]

  if (nrow(filtered) == 0) {
    stop("NAM summary does not contain misperception coefficients.")
  }
  names(filtered)[names(filtered) == "estimate"] <- "current_estimate"
  names(filtered)[names(filtered) == "std_error"] <- "current_std_error"
  names(filtered)[names(filtered) == "t_value"] <- "current_t_value"
  names(filtered)[names(filtered) == "p_value"] <- "current_p_value"
  filtered
}

expectation_frame <- function(expectations) {
  rows <- list()
  idx <- 1
  for (period in names(expectations)) {
    for (term in names(expectations[[period]])) {
      exp <- expectations[[period]][[term]]
      rows[[idx]] <- data.frame(
        time_period = period,
        term = term,
        expected_estimate = exp$estimate,
        tolerance = if (!is.null(exp$tolerance)) exp$tolerance else 0.0,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  do.call(rbind, rows)
}

compare_coefficients <- function(current_df, expectations_df) {
  merged <- merge(current_df, expectations_df, by = c("time_period", "term"), all = TRUE, sort = TRUE)
  merged$estimate_difference <- merged$current_estimate - merged$expected_estimate
  merged$estimate_abs_difference <- abs(merged$estimate_difference)
  merged$estimate_within_tolerance <- merged$estimate_abs_difference <= merged$tolerance
  merged[order(merged$time_period, merged$term), ]
}

enforce_reproduction <- function(comparison_df, synthetic_info = list(present = FALSE, path = NULL)) {
  if (!nrow(comparison_df)) {
    stop("Comparison data frame is empty; cannot evaluate reproduction status.")
  }
  failing <- comparison_df[is.na(comparison_df$estimate_within_tolerance) | !comparison_df$estimate_within_tolerance, , drop = FALSE]
  if (nrow(failing) > 0) {
    if (isTRUE(synthetic_info$present)) {
      message(
        "[chapter5] Proxy-data comparison warning: ", nrow(failing),
        " coefficient checks differ from protected-data targets (maximum absolute difference ",
        sprintf("%.4f", max(failing$estimate_abs_difference, na.rm = TRUE)), "). Synthetic marker: ",
        synthetic_info$path,
        ". This is expected and is not an empirical reproduction claim."
      )
      return(invisible(FALSE))
    }
    message("Reproduction mismatches detected:")
    print(failing[, c("time_period", "term", "current_estimate", "expected_estimate", "estimate_difference", "tolerance")], row.names = FALSE)
    stop("Chapter 5 coefficients diverged beyond tolerance.")
  }
  invisible(TRUE)
}

write_outputs <- function(comparison_df, paths, bundle, current_path, synthetic_info) {
  csv_path <- file.path(paths$tables, "nam_comparison.csv")
  json_path <- file.path(paths$manifests, "nam_comparison.json")

  write.csv(comparison_df, csv_path, row.names = FALSE)

  summary_payload <- list(
    generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    status = if (all(comparison_df$estimate_within_tolerance, na.rm = TRUE)) "passed" else if (isTRUE(synthetic_info$present)) "warning" else "failed",
    synthetic_data = isTRUE(synthetic_info$present),
    current_source = relative_repo_path(current_path, bundle$repo_root),
    rows = nrow(comparison_df),
    metrics = list(
      max_abs_difference = if (nrow(comparison_df) > 0) max(comparison_df$estimate_abs_difference, na.rm = TRUE) else NA_real_,
      within_tolerance = all(comparison_df$estimate_within_tolerance, na.rm = TRUE)
    ),
    comparison = comparison_df
  )

  write_json(summary_payload, json_path, auto_unbox = TRUE, pretty = TRUE)

  list(csv = csv_path, json = json_path)
}

update_checksum_log <- function(bundle, comparison_paths, summary_path, paths) {
  checksum_path <- file.path(paths$logs, "nam_checksums.json")
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)

  tracked <- unique(Filter(
    function(path) !is.null(path) && file.exists(path),
    c(
      summary_path,
      file.path(paths$tables, "nam_plot_data.csv"),
      file.path(paths$figures, "nam_coefficients.png"),
      comparison_paths$csv,
      comparison_paths$json
    )
  ))

  if (length(tracked) == 0) {
    warning("No files found for checksum logging.")
    return(invisible(NULL))
  }

  checksums <- tools::md5sum(tracked)
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
  current_bundle <- load_current_summary(paths)
  expectations <- load_expectations(repo_root)
  synthetic_info <- detect_chapter4_synthetic(bundle)

  current_coeffs <- extract_current_coefficients(current_bundle$data)
  expected_df <- expectation_frame(expectations)

  comparison_df <- compare_coefficients(current_coeffs, expected_df)
  enforce_reproduction(comparison_df, synthetic_info)
  outputs <- write_outputs(comparison_df, paths, bundle, current_bundle$path, synthetic_info)
  checksum_path <- update_checksum_log(bundle, outputs, current_bundle$path, paths)

  message("[chapter5] NAM comparison written to ", outputs$csv)
  message("[chapter5] NAM comparison summary stored at ", outputs$json)
  if (!is.null(checksum_path)) {
    message("[chapter5] Checksum log updated at ", checksum_path)
  }
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error comparing Chapter 5 results: ", e$message)
      quit(status = 1)
    }
  )
}
