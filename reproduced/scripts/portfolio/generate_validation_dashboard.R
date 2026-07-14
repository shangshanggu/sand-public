#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# generate_validation_dashboard.R
#
# Reads chapter outputs (Ch.4 manifests, Ch.5 NAM summary, Ch.7 SAOM
# diagnostics), thesis reference values, and pipeline config, then produces
# a self-contained Markdown validation dashboard at
#   outputs/portfolio/validation_dashboard.md
#
# Missing chapter outputs are marked "not_run" — the script never fails
# because a chapter hasn't been executed yet.  However, thesis.yml MUST
# exist (hard stop per AGENTS.md contract).
#
# Usage (from reproduced/):
#   Rscript scripts/portfolio/generate_validation_dashboard.R
# ---------------------------------------------------------------------------

# ---- bootstrap: locate repo root and load shared helpers ------------------

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches  <- grep(file_arg, cmd_args)
  if (length(matches) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", cmd_args[matches[1]]),
                                 winslash = "/", mustWork = TRUE)))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile,
                                 winslash = "/", mustWork = TRUE)))
  }
  getwd()
}

script_dir   <- get_script_dir()
common_path  <- file.path(script_dir, "..", "..", "R", "common.R")
loader_path  <- file.path(script_dir, "..", "utils", "config_loader.R")

if (file.exists(common_path))  source(common_path)
if (file.exists(loader_path))  source(loader_path)

# ---- load configuration --------------------------------------------------

config_candidates <- c(
  "config/thesis.yml",
  "reproduced/config/thesis.yml"
)
config_path <- NULL
for (cand in config_candidates) {
  if (file.exists(cand)) { config_path <- cand; break }
}
if (is.null(config_path)) {
  stop("thesis.yml not found. Cannot generate validation dashboard.")
}

bundle    <- load_configuration(config_path)
data_mode <- get_config_value(bundle, "data", "mode", default = "real")
env_mode  <- Sys.getenv("SAND_DATA_MODE", "")
if (nzchar(env_mode)) data_mode <- env_mode


# ---- resolve output paths -------------------------------------------------

repo_root   <- bundle$repo_root
repro_root  <- file.path(repo_root, "reproduced")

ch4_manifests_dir  <- file.path(repro_root, "outputs", "chapter4", "manifests")
ch4_data_dir       <- file.path(repro_root, "outputs", "chapter4", "data")
ch5_tables_dir     <- file.path(repro_root, "outputs", "chapter5", "tables")
ch7_logs_dir       <- file.path(repro_root, "outputs", "chapter7", "logs")

nam_summary_path       <- file.path(ch5_tables_dir, "nam_summary.csv")
nam_comparison_path    <- file.path(ch5_tables_dir, "nam_comparison.csv")
saom_diagnostics_path  <- file.path(ch7_logs_dir, "saom_diagnostics.json")
network_arrays_path    <- file.path(ch4_data_dir, "network_arrays.rds")

quan_results_path <- file.path(repro_root, "docs", "references", "quan_results.md")

output_dir  <- file.path(repro_root, "outputs", "portfolio")
output_file <- file.path(output_dir, "validation_dashboard.md")

# ---- helper: null-coalescing operator -------------------------------------

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

# ---- parse thesis reference values from quan_results.md -------------------

parse_quan_results <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  txt   <- paste(lines, collapse = "\n")

  # Extract ```json ... ``` code blocks and find the one with chapter5 expectations
  block_pattern <- "```json\\s*\\n([\\s\\S]*?)\\n```"
  blocks <- regmatches(txt, gregexpr(block_pattern, txt, perl = TRUE))[[1]]
  if (length(blocks) == 0) return(NULL)

  for (block in blocks) {
    if (!grepl("chapter5_nam_expectations", block)) next
    json_str <- sub("^```json\\s*\\n", "", block)
    json_str <- sub("\\n```$", "", json_str)
    result <- tryCatch({
      parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      parsed$chapter5_nam_expectations
    }, error = function(e) NULL)
    if (!is.null(result)) return(result)
  }
  NULL
}

# ---- parse SAOM diagnostics JSON ------------------------------------------

parse_saom_diagnostics <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

# ---- compute Jaccard indices from network arrays --------------------------

compute_jaccard_indices <- function(arrays_path) {
  if (!file.exists(arrays_path)) return(NULL)
  arrays <- tryCatch(readRDS(arrays_path), error = function(e) NULL)
  if (is.null(arrays)) return(NULL)

  # arrays is expected to be a 3D array: n x n x waves
  # or a list of matrices. Handle both.
  mats <- NULL
  if (is.array(arrays) && length(dim(arrays)) == 3) {
    n_waves <- dim(arrays)[3]
    wave_names <- dimnames(arrays)[[3]]
    mats <- lapply(seq_len(n_waves), function(i) arrays[, , i])
    if (!is.null(wave_names)) names(mats) <- wave_names
  } else if (is.list(arrays) &&
             !is.null(arrays$network_array) &&
             is.array(arrays$network_array) &&
             length(dim(arrays$network_array)) == 3) {
    net_arr <- arrays$network_array
    n_waves <- dim(net_arr)[3]
    wave_names <- dimnames(net_arr)[[3]]
    mats <- lapply(seq_len(n_waves), function(i) net_arr[, , i])
    if (!is.null(wave_names)) names(mats) <- wave_names
  } else if (is.list(arrays)) {
    # Keep only matrix-like entries if available.
    mats <- Filter(function(x) is.matrix(x) || (is.array(x) && length(dim(x)) == 2), arrays)
  }

  if (is.null(mats) || length(mats) < 2) return(NULL)

  # Convert a wave adjacency matrix to a stable directed edge set. This avoids
  # non-conformable array issues when wave matrices differ in dimension.
  matrix_to_edge_set <- function(mat) {
    m <- as.matrix(mat)
    idx <- which(m != 0, arr.ind = TRUE)
    if (nrow(idx) == 0) {
      return(character(0))
    }
    rn <- rownames(m)
    cn <- colnames(m)
    if (is.null(rn)) rn <- as.character(seq_len(nrow(m)))
    if (is.null(cn)) cn <- as.character(seq_len(ncol(m)))
    paste0(rn[idx[, 1]], "->", cn[idx[, 2]])
  }

  # SAOM-aligned waves: 2, 4, 5, 6
  saom_waves <- c("2", "4", "5", "6")
  wave_labels <- names(mats)

  # Chapter 4 stores the four selected SAOM waves as wave1..wave4. Restore
  # their study-wave labels here so the public dashboard does not imply that
  # Waves 1 and 3 were part of the SAOM panel.
  if (length(mats) == 4 && identical(wave_labels, paste0("wave", seq_len(4)))) {
    wave_labels <- saom_waves
    names(mats) <- wave_labels
  }

  # Try to match wave names; fall back to positional if unnamed
  if (!is.null(wave_labels)) {
    idx <- match(saom_waves, wave_labels)
    idx <- idx[!is.na(idx)]
    if (length(idx) < 2) {
      # Try numeric matching
      idx <- which(wave_labels %in% saom_waves)
    }
    if (length(idx) >= 2) {
      mats <- mats[idx]
      wave_labels <- wave_labels[idx]
    }
  } else {
    wave_labels <- as.character(seq_along(mats))
  }

  # Compute Jaccard for consecutive pairs
  results <- list()
  for (i in seq_len(length(mats) - 1)) {
    e1 <- matrix_to_edge_set(mats[[i]])
    e2 <- matrix_to_edge_set(mats[[i + 1]])
    # Jaccard = |intersection(edges)| / |union(edges)|
    intersection <- length(intersect(e1, e2))
    union_count  <- length(union(e1, e2))
    jaccard <- if (union_count > 0) intersection / union_count else NA_real_
    pair_label <- paste0(wave_labels[i], "\u2192", wave_labels[i + 1])
    results[[length(results) + 1]] <- list(
      wave_pair     = pair_label,
      jaccard_index = jaccard
    )
  }
  results
}

# ---- check chapter 4 status ----------------------------------------------

check_chapter4 <- function() {
  has_manifests <- dir.exists(ch4_manifests_dir) &&
    length(list.files(ch4_manifests_dir, pattern = "\\.json$")) > 0
  has_data <- file.exists(network_arrays_path)

  if (!has_manifests && !has_data) {
    return(list(chapter = "chapter4_data_collection",
                status = "not_run", checks = list()))
  }

  checks <- list()
  if (has_manifests) {
    manifest_files <- list.files(ch4_manifests_dir, pattern = "\\.json$",
                                 full.names = TRUE)
    checks[[length(checks) + 1]] <- list(
      name = "data_preparation_manifests",
      metrics = list(list(
        term = "manifest_count",
        expected = NA, actual = length(manifest_files),
        tolerance = NA, passed = length(manifest_files) > 0
      ))
    )
  }

  status <- if (all(vapply(checks, function(c) {
    all(vapply(c$metrics, function(m) isTRUE(m$passed), logical(1)))
  }, logical(1)))) "pass" else "fail"

  list(chapter = "chapter4_data_collection", status = status, checks = checks)
}


# ---- check chapter 5 status (NAM coefficients vs thesis) ------------------

check_chapter5 <- function(thesis_expectations, mode = data_mode) {
  ch5_tables <- get0(
    "ch5_tables_dir",
    ifnotfound = file.path(repro_root, "outputs", "chapter5", "tables"),
    inherits = TRUE
  )
  local_nam_summary_path <- get0(
    "nam_summary_path",
    ifnotfound = file.path(ch5_tables, "nam_summary.csv"),
    inherits = TRUE
  )
  local_nam_comparison_path <- get0(
    "nam_comparison_path",
    ifnotfound = file.path(ch5_tables, "nam_comparison.csv"),
    inherits = TRUE
  )

  if (!file.exists(local_nam_summary_path) && !file.exists(local_nam_comparison_path)) {
    return(list(chapter = "chapter5_descriptive_norms",
                status = "not_run", checks = list()))
  }

  nam_df <- NULL
  if (file.exists(local_nam_comparison_path)) {
    nam_df <- tryCatch(
      read.csv(local_nam_comparison_path, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(nam_df) && file.exists(local_nam_summary_path)) {
    nam_df <- tryCatch(
      read.csv(local_nam_summary_path, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(nam_df)) {
    return(list(chapter = "chapter5_descriptive_norms",
                status = "fail",
                checks = list(list(
                  name = "nam_coefficients",
                  metrics = list(list(
                    term = "file_read", expected = "readable",
                    actual = "parse_error", tolerance = NA, passed = FALSE
                  ))
                ))))
  }

  expected_term_aliases <- function(term_name) {
    if (identical(term_name, "global_misperception")) {
      return(c("global_misperception",
               "misperception_audit_c_global",
               "Global-level misperception"))
    }
    if (identical(term_name, "peer_misperception")) {
      return(c("peer_misperception",
               "misperception_audit_c_peer",
               "Peer-level misperception"))
    }
    term_name
  }

  estimate_column <- "estimate"
  if ("current_estimate" %in% names(nam_df)) {
    estimate_column <- "current_estimate"
  }

  term_columns <- intersect(c("term", "term_raw", "term_label"), names(nam_df))

  metrics <- list()
  if (!is.null(thesis_expectations)) {
    for (tp_name in names(thesis_expectations)) {
      tp <- thesis_expectations[[tp_name]]
      for (term_name in names(tp)) {
        expected_val <- as.numeric(tp[[term_name]]$estimate)
        tolerance    <- as.numeric(tp[[term_name]]$tolerance %||% 1e-04)
        aliases      <- expected_term_aliases(term_name)

        match_rows <- data.frame()
        if ("time_period" %in% names(nam_df) && length(term_columns) > 0) {
          for (term_col in term_columns) {
            rows <- nam_df[nam_df$time_period == tp_name &
                             nam_df[[term_col]] %in% aliases, , drop = FALSE]
            if (nrow(rows) > 0) {
              match_rows <- rows
              break
            }
          }
        }

        if (nrow(match_rows) == 0) {
          metrics[[length(metrics) + 1]] <- list(
            time_period = tp_name, term = term_name,
            expected = expected_val, actual = NA,
            tolerance = tolerance, passed = FALSE
          )
        } else {
          actual_val <- as.numeric(match_rows[[estimate_column]][1])
          passed <- !is.na(actual_val) &&
            abs(actual_val - expected_val) <= tolerance
          metrics[[length(metrics) + 1]] <- list(
            time_period = tp_name, term = term_name,
            expected = expected_val, actual = actual_val,
            tolerance = tolerance, passed = passed
          )
        }
      }
    }
  }

  all_passed <- length(metrics) > 0 &&
    all(vapply(metrics, function(m) isTRUE(m$passed), logical(1)))

  list(
    chapter = "chapter5_descriptive_norms",
    status  = if (length(metrics) == 0 || all_passed) {
      "pass"
    } else if (identical(mode, "proxy")) {
      "warning"
    } else {
      "fail"
    },
    checks  = list(list(name = "nam_coefficients", metrics = metrics))
  )
}

# ---- check chapter 7 status (SAOM convergence) ----------------------------

check_chapter7 <- function(mode = data_mode) {
  if (!file.exists(saom_diagnostics_path)) {
    return(list(chapter = "chapter7_saom",
                status = "not_run", checks = list()))
  }

  diag <- parse_saom_diagnostics(saom_diagnostics_path)
  if (is.null(diag)) {
    return(list(chapter = "chapter7_saom",
                status = "fail",
                checks = list(list(
                  name = "saom_convergence",
                  metrics = list(list(
                    term = "file_read", expected = "readable",
                    actual = "parse_error", tolerance = NA, passed = FALSE
                  ))
                ))))
  }

  metrics <- list()

  # Overall convergence ratio (tconv.max)
  tconv_max <- diag$tconv.max %||% diag$tconv_max %||% diag$overall_max_convergence
  if (!is.null(tconv_max)) {
    tconv_val <- as.numeric(tconv_max)
    # Convergence is good when tconv.max < 0.25
    metrics[[length(metrics) + 1]] <- list(
      term = "tconv.max", expected = 0.25,
      actual = tconv_val, tolerance = NA,
      passed = !is.na(tconv_val) && tconv_val < 0.25
    )
  }

  # Per-parameter t-ratios
  t_ratios <- diag$t_ratios %||% diag$parameter_t_ratios %||% diag$convergence
  if (is.list(t_ratios)) {
    for (param_name in names(t_ratios)) {
      t_val <- as.numeric(t_ratios[[param_name]])
      # Good convergence: |t-ratio| < 0.1
      metrics[[length(metrics) + 1]] <- list(
        term = param_name, expected = 0.1,
        actual = t_val, tolerance = NA,
        passed = !is.na(t_val) && abs(t_val) < 0.1
      )
    }
  }

  all_passed <- length(metrics) > 0 &&
    all(vapply(metrics, function(m) isTRUE(m$passed), logical(1)))

  list(
    chapter = "chapter7_saom",
    status  = if (length(metrics) == 0 || all_passed) {
      "pass"
    } else if (identical(mode, "proxy")) {
      "warning"
    } else {
      "fail"
    },
    checks  = list(list(name = "saom_convergence", metrics = metrics))
  )
}

# ---- build the full report data structure ---------------------------------

build_report <- function() {
  thesis_expectations <- parse_quan_results(quan_results_path)

  ch4_result <- check_chapter4()
  ch5_result <- check_chapter5(thesis_expectations, data_mode)
  ch7_result <- check_chapter7(data_mode)

  jaccard_entries <- compute_jaccard_indices(network_arrays_path) %||% list()

  chapters <- list(ch4_result, ch5_result, ch7_result)

  run_chapters <- Filter(function(c) c$status != "not_run", chapters)
  run_statuses <- vapply(run_chapters, function(c) c$status, character(1))
  overall_status <- if (length(run_chapters) == 0) {
    "not_run"
  } else if (any(run_statuses == "fail")) {
    "fail"
  } else if (any(run_statuses == "warning")) {
    "warning"
  } else {
    "pass"
  }

  list(
    generated_at      = format_timestamp(),
    data_mode         = data_mode,
    chapters          = chapters,
    network_stability = jaccard_entries,
    overall_status    = overall_status,
    overall_passed    = overall_status %in% c("pass", "warning")
  )
}


# ---- render Markdown dashboard --------------------------------------------

status_emoji <- function(status) {
  switch(status,
    pass    = "\u2705",
    warning = "\u26a0\ufe0f",
    fail    = "\u274c",
    not_run = "\u23f8\ufe0f",
    "\u2753"
  )
}

render_pipeline_overview <- function(report) {
  chapters_summary <- vapply(report$chapters, function(c) {
    sprintf("| %s | %s %s |", c$chapter, status_emoji(c$status), c$status)
  }, character(1))

  c(
    "## Pipeline Overview",
    "",
    sprintf("- **Generated:** %s", report$generated_at),
    sprintf("- **Data mode:** %s", report$data_mode),
    sprintf("- **Overall status:** %s %s",
            status_emoji(report$overall_status),
            overall_label(report)),
    "",
    "| Chapter | Status |",
    "|---------|--------|",
    chapters_summary,
    ""
  )
}

render_chapter4 <- function(ch4) {
  if (ch4$status == "not_run") {
    return(c("## Chapter 4: Data Preparation", "",
             paste(status_emoji("not_run"), "Chapter 4 has not been run."), ""))
  }
  lines <- c(
    "## Chapter 4: Data Preparation", "",
    sprintf("%s Status: **%s**", status_emoji(ch4$status), toupper(ch4$status)), ""
  )
  for (check in ch4$checks) {
    for (m in check$metrics) {
      lines <- c(lines, sprintf("- %s: %s", m$term, m$actual))
    }
  }
  c(lines, "")
}

render_chapter5 <- function(ch5) {
  if (ch5$status == "not_run") {
    return(c("## Chapter 5: NAM Coefficients", "",
             paste(status_emoji("not_run"), "Chapter 5 has not been run."), ""))
  }
  lines <- c(
    "## Chapter 5: NAM Coefficients", "",
    sprintf("%s Status: **%s**", status_emoji(ch5$status), toupper(ch5$status)), ""
  )
  if (identical(data_mode, "proxy")) {
    lines <- c(
      lines,
      "Proxy coefficients are shown against thesis references as a diagnostic only;",
      "a mismatch is expected and is not an empirical validation failure.",
      ""
    )
  }

  # Find the nam_coefficients check
  nam_check <- NULL
  for (check in ch5$checks) {
    if (check$name == "nam_coefficients") { nam_check <- check; break }
  }

  if (!is.null(nam_check) && length(nam_check$metrics) > 0) {
    lines <- c(lines,
      "| Time Period | Term | Expected | Actual | Tolerance | Result |",
      "|-------------|------|----------|--------|-----------|--------|"
    )
    for (m in nam_check$metrics) {
      expected_str <- if (is.na(m$expected)) "N/A" else sprintf("%.4f", m$expected)
      actual_str   <- if (is.null(m$actual) || is.na(m$actual)) "N/A" else sprintf("%.4f", m$actual)
      tol_str      <- if (is.na(m$tolerance)) "N/A" else sprintf("%.0e", m$tolerance)
      result_str   <- if (isTRUE(m$passed)) "\u2705" else if (identical(data_mode, "proxy")) "\u26a0\ufe0f" else "\u274c"
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s |",
                                m$time_period %||% "", m$term %||% "",
                                expected_str, actual_str, tol_str, result_str))
    }
    lines <- c(lines, "")
  }
  lines
}

render_chapter7 <- function(ch7) {
  if (ch7$status == "not_run") {
    return(c("## Chapter 7: SAOM Convergence Diagnostics", "",
             paste(status_emoji("not_run"), "Chapter 7 has not been run."), ""))
  }
  lines <- c(
    "## Chapter 7: SAOM Convergence Diagnostics", "",
    sprintf("%s Status: **%s**", status_emoji(ch7$status), toupper(ch7$status)), ""
  )
  if (identical(data_mode, "proxy")) {
    lines <- c(
      lines,
      "Convergence diagnostics from proxy data describe this demonstration fit;",
      "they do not validate the thesis model or empirical findings.",
      ""
    )
  }

  conv_check <- NULL
  for (check in ch7$checks) {
    if (check$name == "saom_convergence") { conv_check <- check; break }
  }

  if (!is.null(conv_check) && length(conv_check$metrics) > 0) {
    # Separate tconv.max from per-parameter t-ratios
    tconv_metrics <- Filter(function(m) m$term == "tconv.max", conv_check$metrics)
    param_metrics <- Filter(function(m) m$term != "tconv.max", conv_check$metrics)

    if (length(tconv_metrics) > 0) {
      m <- tconv_metrics[[1]]
      lines <- c(lines,
        sprintf("**Overall convergence ratio (tconv.max):** %.4f %s (threshold: < %.2f)",
                m$actual,
                if (isTRUE(m$passed)) "\u2705" else if (identical(data_mode, "proxy")) "\u26a0\ufe0f" else "\u274c",
                m$expected),
        ""
      )
    }

    if (length(param_metrics) > 0) {
      lines <- c(lines,
        "### Per-Parameter t-Ratios", "",
        "| Parameter | t-ratio | |t| < 0.1 |",
        "|-----------|---------|-----------|"
      )
      for (m in param_metrics) {
        actual_str <- if (is.na(m$actual)) "N/A" else sprintf("%.4f", m$actual)
        result_str <- if (isTRUE(m$passed)) "\u2705" else if (identical(data_mode, "proxy")) "\u26a0\ufe0f" else "\u274c"
        lines <- c(lines, sprintf("| %s | %s | %s |", m$term, actual_str, result_str))
      }
      lines <- c(lines, "")
    }
  }
  lines
}

render_jaccard <- function(jaccard_entries) {
  lines <- c("## Network Stability: Jaccard Indices", "")
  if (length(jaccard_entries) == 0) {
    return(c(lines, paste(status_emoji("not_run"),
             "Network arrays not available; Jaccard indices not computed."), ""))
  }
  lines <- c(lines,
    "| Wave Pair | Jaccard Index |",
    "|-----------|---------------|"
  )
  for (entry in jaccard_entries) {
    j_str <- if (is.na(entry$jaccard_index)) "N/A" else sprintf("%.4f", entry$jaccard_index)
    lines <- c(lines, sprintf("| %s | %s |", entry$wave_pair, j_str))
  }
  c(lines, "",
    "*Jaccard index measures the proportion of ties present in both waves",
    "relative to ties present in either wave. Higher values indicate greater",
    "network stability.*", "")
}

overall_label <- function(report) {
  if (report$overall_status == "not_run") return("NOT RUN")
  if (report$data_mode == "proxy") {
    return(switch(
      report$overall_status,
      pass = "STRUCTURAL PASS",
      warning = "STRUCTURAL PASS WITH WARNINGS",
      fail = "STRUCTURAL FAIL"
    ))
  }
  toupper(report$overall_status)
}

render_overall <- function(report) {
  run_count    <- sum(vapply(report$chapters, function(c) c$status != "not_run", logical(1)))
  pass_count   <- sum(vapply(report$chapters, function(c) c$status == "pass", logical(1)))
  warning_count <- sum(vapply(report$chapters, function(c) c$status == "warning", logical(1)))
  fail_count   <- sum(vapply(report$chapters, function(c) c$status == "fail", logical(1)))
  notrun_count <- sum(vapply(report$chapters, function(c) c$status == "not_run", logical(1)))

  c(
    "## Overall Summary", "",
    sprintf("- Chapters executed: %d", run_count),
    sprintf("- Passed: %d", pass_count),
    sprintf("- Warnings: %d", warning_count),
    sprintf("- Failed: %d", fail_count),
    sprintf("- Not run: %d", notrun_count),
    "",
    sprintf("**Overall result: %s %s**",
            status_emoji(report$overall_status),
            overall_label(report)),
    ""
  )
}

render_dashboard <- function(report) {
  ch4 <- ch5 <- ch7 <- NULL
  for (ch in report$chapters) {
    if (ch$chapter == "chapter4_data_collection")    ch4 <- ch
    if (ch$chapter == "chapter5_descriptive_norms")  ch5 <- ch
    if (ch$chapter == "chapter7_saom")               ch7 <- ch
  }

  proxy_banner <- character(0)
  if (report$data_mode == "proxy") {
    proxy_banner <- c(
      "> **\u26a0\ufe0f PROXY DATA** — This dashboard was generated from synthetic proxy inputs.",
      "> Coefficients are not expected to match thesis reference values.",
      "> Run with real staged data (`make all`) to produce validated results.",
      ""
    )
  }

  lines <- c(
    "# Validation Dashboard", "",
    proxy_banner,
    render_pipeline_overview(report),
    render_chapter4(ch4),
    render_chapter5(ch5),
    render_chapter7(ch7),
    render_jaccard(report$network_stability),
    render_overall(report)
  )
  lines
}

# ---- main -----------------------------------------------------------------

main <- function() {
  message("[portfolio-dashboard] Building validation report...")

  report <- build_report()

  # Ensure output directory exists
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Render and write
  md_lines <- render_dashboard(report)
  writeLines(md_lines, output_file, useBytes = TRUE)

  message(sprintf("[portfolio-dashboard] Dashboard written to %s", output_file))
  message(sprintf("[portfolio-dashboard] Overall: %s", overall_label(report)))
  invisible(report)
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
