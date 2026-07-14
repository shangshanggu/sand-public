# ---------------------------------------------------------------------------
# Property tests for the Validation Dashboard Generator (Properties 1–4)
#
# Feature: portfolio-packaging
# Validates: Requirements 3.1, 3.2, 3.3, 3.4
# ---------------------------------------------------------------------------

library(testthat)
library(jsonlite)

# ---- Resolve the real reproduced/ root ------------------------------------

.find_repro_root <- function() {
  if (exists("REPO_ROOT") &&
      file.exists(file.path(REPO_ROOT, "config", "thesis.yml"))) {
    return(REPO_ROOT)
  }
  d <- getwd()
  for (i in 1:10) {
    if (file.exists(file.path(d, "config", "thesis.yml"))) return(d)
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }
  stop("Cannot locate reproduced/ root")
}

REPRO_ROOT <- .find_repro_root()

# ---- Load dashboard functions without executing module-level code ----------
#
# Strategy: parse the dashboard script, extract only function/operator
# assignments, and evaluate them in a controlled environment that already
# has the shared helpers loaded.

.load_dashboard_functions <- function() {
  script_path <- file.path(REPRO_ROOT, "scripts", "portfolio",
                           "generate_validation_dashboard.R")
  stopifnot(file.exists(script_path))

  # Parse the script into an expression list
  exprs <- parse(script_path)

  env <- new.env(parent = globalenv())

  # Source shared helpers
  source(file.path(REPRO_ROOT, "R", "common.R"), local = env)
  source(file.path(REPRO_ROOT, "scripts", "utils", "config_loader.R"), local = env)

  # Evaluate only function assignments and operator definitions.
  # We skip everything else (module-level calls like source(), load_configuration(), etc.)
  for (expr in exprs) {
    # Check if this is an assignment: `name <- function(...)` or `name <- value`
    # or an operator definition like `%||%` <- function(...)
    expr_text <- deparse(expr, width.cutoff = 500L)
    first_line <- expr_text[1]

    is_assignment <- grepl("^(`[^`]+`|[A-Za-z_.][A-Za-z0-9_.]*) *(<-|=) *function\\b",
                           first_line)
    is_operator   <- grepl("^`%", first_line) && grepl("<-", first_line)

    if (is_assignment || is_operator) {
      eval(expr, envir = env)
    }
  }

  env
}

# Load once — these are pure functions that don't depend on module-level state
# (except for the check_* functions which read module-level path variables,
# but we override those per-test via the env).
DASH_ENV <- .load_dashboard_functions()

# ---- Fixture helpers -------------------------------------------------------

create_fixture_tree <- function() {
  base <- tempfile(pattern = "dash_test_")
  repro <- file.path(base, "reproduced")
  config_dir <- file.path(repro, "config")
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)

  yaml_content <- list(
    project = list(
      name = "test-project",
      environment = list(conda_env = "r_stable"),
      paths = list(raw_data_dir = "reproduced/data/raw")
    ),
    data = list(mode = "proxy", proxy_dir = "reproduced/data/proxy"),
    chapters = list(
      chapter4_data_collection = list(
        enabled = TRUE, outputs_dir = "reproduced/outputs/chapter4"
      ),
      chapter5_descriptive_norms = list(
        enabled = TRUE, outputs_dir = "reproduced/outputs/chapter5"
      ),
      chapter7_saom = list(
        enabled = TRUE, outputs_dir = "reproduced/outputs/chapter7"
      )
    )
  )
  yaml::write_yaml(yaml_content, file.path(config_dir, "thesis.yml"))

  for (ch in c("chapter4", "chapter5", "chapter7")) {
    for (sub in c("manifests", "tables", "logs", "data")) {
      dir.create(file.path(repro, "outputs", ch, sub),
                 recursive = TRUE, showWarnings = FALSE)
    }
  }
  dir.create(file.path(repro, "docs", "references"),
             recursive = TRUE, showWarnings = FALSE)
  base
}

add_ch4_manifests <- function(repro) {
  d <- file.path(repro, "outputs", "chapter4", "manifests")
  jsonlite::write_json(
    list(chapter = "chapter4", status = "complete",
         timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
    file.path(d, "data_prep.json"), auto_unbox = TRUE, pretty = TRUE
  )
}

add_ch5_nam <- function(repro) {
  d <- file.path(repro, "outputs", "chapter5", "tables")
  write.csv(data.frame(
    time_period = c("time1", "time1", "time2", "time2"),
    term        = c("intercept", "network_effect",
                    "intercept", "network_effect"),
    estimate    = c(1.234, 0.567, 1.345, 0.678),
    std_error   = c(0.1, 0.05, 0.12, 0.06),
    stringsAsFactors = FALSE
  ), file.path(d, "nam_summary.csv"), row.names = FALSE)
}

add_ch7_diagnostics <- function(repro) {
  d <- file.path(repro, "outputs", "chapter7", "logs")
  jsonlite::write_json(list(
    tconv.max = 0.0624,
    t_ratios = list(avSim = 0.023, density = -0.045, reciprocity = 0.012)
  ), file.path(d, "saom_diagnostics.json"), auto_unbox = TRUE, pretty = TRUE)
}

add_network_arrays <- function(repro, n = 10,
                               waves = c("2", "4", "5", "6")) {
  d <- file.path(repro, "outputs", "chapter4", "data")
  arr <- array(0L, dim = c(n, n, length(waves)),
               dimnames = list(NULL, NULL, waves))
  set.seed(42)
  for (w in seq_along(waves)) {
    ties <- matrix(rbinom(n * n, 1, 0.2), nrow = n, ncol = n)
    diag(ties) <- 0
    arr[, , w] <- ties
  }
  saveRDS(arr, file.path(d, "network_arrays.rds"))
}

add_quan_results <- function(repro) {
  d <- file.path(repro, "docs", "references")
  content <- c(
    "# Quantitative Results Reference", "",
    "```json",
    jsonlite::toJSON(list(
      chapter5_nam_expectations = list(
        time1 = list(
          intercept = list(estimate = 1.234, tolerance = 0.01),
          network_effect = list(estimate = 0.567, tolerance = 0.01)
        ),
        time2 = list(
          intercept = list(estimate = 1.345, tolerance = 0.01),
          network_effect = list(estimate = 0.678, tolerance = 0.01)
        )
      )
    ), auto_unbox = TRUE, pretty = TRUE),
    "```", ""
  )
  writeLines(content, file.path(d, "quan_results.md"))
}

# ---- Build a report from fixtures -----------------------------------------
#
# We clone the DASH_ENV for each test run and override the module-level
# path variables so the check_* and build_report functions read from
# our fixture tree.

run_dashboard_in_fixtures <- function(include_ch4 = TRUE,
                                      include_ch5 = TRUE,
                                      include_ch7 = TRUE,
                                      include_arrays = TRUE,
                                      include_quan = TRUE) {
  base <- create_fixture_tree()
  repro <- file.path(base, "reproduced")
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  if (include_ch4)    add_ch4_manifests(repro)
  if (include_ch5)    add_ch5_nam(repro)
  if (include_ch7)    add_ch7_diagnostics(repro)
  if (include_arrays) add_network_arrays(repro)
  if (include_quan)   add_quan_results(repro)

  # Clone the dashboard env and re-bind function environments so they
  # see the overridden path variables in the new env.
  env <- new.env(parent = parent.env(DASH_ENV))
  for (nm in ls(DASH_ENV, all.names = TRUE)) {
    obj <- get(nm, envir = DASH_ENV)
    if (is.function(obj)) {
      # Re-bind the function's closure to the new env
      environment(obj) <- env
    }
    assign(nm, obj, envir = env)
  }

  env$repro_root            <- repro
  env$repo_root             <- base
  env$ch4_manifests_dir     <- file.path(repro, "outputs", "chapter4", "manifests")
  env$ch4_data_dir          <- file.path(repro, "outputs", "chapter4", "data")
  env$ch5_tables_dir        <- file.path(repro, "outputs", "chapter5", "tables")
  env$ch7_logs_dir          <- file.path(repro, "outputs", "chapter7", "logs")
  env$nam_summary_path      <- file.path(repro, "outputs", "chapter5", "tables",
                                         "nam_summary.csv")
  env$saom_diagnostics_path <- file.path(repro, "outputs", "chapter7", "logs",
                                         "saom_diagnostics.json")
  env$network_arrays_path   <- file.path(repro, "outputs", "chapter4", "data",
                                         "network_arrays.rds")
  env$quan_results_path     <- file.path(repro, "docs", "references",
                                         "quan_results.md")
  env$data_mode             <- "proxy"

  env$build_report()
}


# ===========================================================================
# Property 1: Validation dashboard contains per-chapter status for all
#             executed chapters
# Feature: portfolio-packaging, Property 1: Validation dashboard contains
#   per-chapter status for all executed chapters
# **Validates: Requirements 3.1**
# ===========================================================================

test_that("Property 1: dashboard contains per-chapter status for all executed chapters", {
  report <- run_dashboard_in_fixtures(
    include_ch4 = TRUE, include_ch5 = TRUE, include_ch7 = TRUE,
    include_arrays = TRUE, include_quan = TRUE
  )

  expect_true(is.list(report))
  expect_true("chapters" %in% names(report))
  expect_true(length(report$chapters) >= 3)

  valid_statuses <- c("pass", "warning", "fail", "not_run")
  for (ch in report$chapters) {
    expect_true("chapter" %in% names(ch),
                info = "Each chapter entry must have a 'chapter' field")
    expect_true("status" %in% names(ch),
                info = paste("Chapter", ch$chapter, "must have 'status'"))
    expect_true(ch$status %in% valid_statuses,
                info = paste("Status for", ch$chapter, "must be valid"))
  }

  # Timestamp and data_mode
  expect_true("generated_at" %in% names(report))
  expect_true(nzchar(report$generated_at))
  expect_true("data_mode" %in% names(report))
  expect_true(report$data_mode %in% c("real", "proxy"))
})

test_that("Property 1: chapters with outputs get pass/fail, missing get not_run", {
  report <- run_dashboard_in_fixtures(
    include_ch4 = TRUE, include_ch5 = FALSE, include_ch7 = FALSE,
    include_arrays = FALSE, include_quan = FALSE
  )

  ch_map <- setNames(
    lapply(report$chapters, function(c) c$status),
    vapply(report$chapters, function(c) c$chapter, character(1))
  )

  expect_equal(ch_map[["chapter4_data_collection"]], "pass")
  expect_equal(ch_map[["chapter5_descriptive_norms"]], "not_run")
  expect_equal(ch_map[["chapter7_saom"]], "not_run")
})


# ===========================================================================
# Property 2: Validation dashboard includes SAOM convergence diagnostics
#             when Ch.7 outputs exist
# Feature: portfolio-packaging, Property 2: Validation dashboard includes
#   SAOM convergence diagnostics when Ch.7 outputs exist
# **Validates: Requirements 3.2**
# ===========================================================================

test_that("Property 2: dashboard includes tconv.max and t-ratios when Ch.7 exists", {
  report <- run_dashboard_in_fixtures(
    include_ch4 = FALSE, include_ch5 = FALSE, include_ch7 = TRUE,
    include_arrays = FALSE, include_quan = FALSE
  )

  ch7 <- NULL
  for (ch in report$chapters) {
    if (ch$chapter == "chapter7_saom") { ch7 <- ch; break }
  }
  expect_false(is.null(ch7))
  expect_true(ch7$status != "not_run")

  conv_check <- NULL
  for (check in ch7$checks) {
    if (check$name == "saom_convergence") { conv_check <- check; break }
  }
  expect_false(is.null(conv_check))

  terms <- vapply(conv_check$metrics, function(m) m$term, character(1))
  expect_true("tconv.max" %in% terms)

  param_terms <- terms[terms != "tconv.max"]
  expect_true(length(param_terms) >= 1,
              info = "Must include at least one per-parameter t-ratio")
})

test_that("Property 2: Ch.7 is not_run when diagnostics file is absent", {
  report <- run_dashboard_in_fixtures(
    include_ch4 = FALSE, include_ch5 = FALSE, include_ch7 = FALSE,
    include_arrays = FALSE, include_quan = FALSE
  )

  ch7 <- NULL
  for (ch in report$chapters) {
    if (ch$chapter == "chapter7_saom") { ch7 <- ch; break }
  }
  expect_false(is.null(ch7))
  expect_equal(ch7$status, "not_run")
})


# ===========================================================================
# Property 3: Validation dashboard pairs NAM coefficients with thesis
#             reference values
# Feature: portfolio-packaging, Property 3: Validation dashboard pairs NAM
#   coefficients with thesis reference values
# **Validates: Requirements 3.3**
# ===========================================================================

test_that("Property 3: each NAM coefficient row includes actual and expected values", {
  report <- run_dashboard_in_fixtures(
    include_ch4 = FALSE, include_ch5 = TRUE, include_ch7 = FALSE,
    include_arrays = FALSE, include_quan = TRUE
  )

  ch5 <- NULL
  for (ch in report$chapters) {
    if (ch$chapter == "chapter5_descriptive_norms") { ch5 <- ch; break }
  }
  expect_false(is.null(ch5))
  expect_true(ch5$status != "not_run")

  nam_check <- NULL
  for (check in ch5$checks) {
    if (check$name == "nam_coefficients") { nam_check <- check; break }
  }
  expect_false(is.null(nam_check))
  expect_true(length(nam_check$metrics) > 0)

  for (m in nam_check$metrics) {
    expect_true("expected" %in% names(m),
                info = paste("Metric for", m$term, "must have 'expected'"))
    expect_true("actual" %in% names(m),
                info = paste("Metric for", m$term, "must have 'actual'"))
    expect_true(is.numeric(m$expected))
    expect_true(is.numeric(m$actual))
  }
})

test_that("Property 3: NAM metrics cover all thesis reference terms", {
  report <- run_dashboard_in_fixtures(
    include_ch4 = FALSE, include_ch5 = TRUE, include_ch7 = FALSE,
    include_arrays = FALSE, include_quan = TRUE
  )

  ch5 <- NULL
  for (ch in report$chapters) {
    if (ch$chapter == "chapter5_descriptive_norms") { ch5 <- ch; break }
  }
  nam_check <- NULL
  for (check in ch5$checks) {
    if (check$name == "nam_coefficients") { nam_check <- check; break }
  }

  expect_equal(length(nam_check$metrics), 4)

  metric_keys <- vapply(nam_check$metrics, function(m) {
    paste(m$time_period, m$term, sep = ":")
  }, character(1))
  expect_true("time1:intercept" %in% metric_keys)
  expect_true("time1:network_effect" %in% metric_keys)
  expect_true("time2:intercept" %in% metric_keys)
  expect_true("time2:network_effect" %in% metric_keys)
})


# ===========================================================================
# Property 4: Validation dashboard includes Jaccard indices for all
#             consecutive wave pairs
# Feature: portfolio-packaging, Property 4: Validation dashboard includes
#   Jaccard indices for all consecutive wave pairs
# **Validates: Requirements 3.4**
# ===========================================================================

test_that("Property 4: Jaccard entries exist for wave pairs 2->4, 4->5, 5->6", {
  report <- run_dashboard_in_fixtures(
    include_ch4 = TRUE, include_ch5 = FALSE, include_ch7 = FALSE,
    include_arrays = TRUE, include_quan = FALSE
  )

  expect_true("network_stability" %in% names(report))
  jaccard <- report$network_stability
  expect_true(length(jaccard) >= 3)

  pair_labels <- vapply(jaccard, function(e) e$wave_pair, character(1))
  expect_true(any(grepl("2.*4", pair_labels)),
              info = "Must include Jaccard for wave pair 2->4")
  expect_true(any(grepl("4.*5", pair_labels)),
              info = "Must include Jaccard for wave pair 4->5")
  expect_true(any(grepl("5.*6", pair_labels)),
              info = "Must include Jaccard for wave pair 5->6")
})

test_that("Property 4: each Jaccard entry has a numeric index in [0, 1]", {
  report <- run_dashboard_in_fixtures(
    include_ch4 = TRUE, include_ch5 = FALSE, include_ch7 = FALSE,
    include_arrays = TRUE, include_quan = FALSE
  )

  for (entry in report$network_stability) {
    expect_true("wave_pair" %in% names(entry))
    expect_true("jaccard_index" %in% names(entry))
    expect_true(is.numeric(entry$jaccard_index))
    expect_true(entry$jaccard_index >= 0 && entry$jaccard_index <= 1,
                info = paste("Jaccard for", entry$wave_pair, "must be in [0,1]"))
  }
})

test_that("Property 4: generic four-slice labels map to study waves 2, 4, 5, and 6", {
  base <- tempfile("sand-jaccard-")
  dir.create(base, recursive = TRUE)
  on.exit(unlink(base, recursive = TRUE), add = TRUE)
  array_path <- file.path(base, "network_arrays.rds")
  arr <- array(
    0L,
    dim = c(3, 3, 4),
    dimnames = list(as.character(1:3), as.character(1:3), paste0("wave", 1:4))
  )
  arr[1, 2, ] <- 1L
  saveRDS(arr, array_path)

  entries <- DASH_ENV$compute_jaccard_indices(array_path)
  expect_equal(
    vapply(entries, function(entry) entry$wave_pair, character(1)),
    c("2\u21924", "4\u21925", "5\u21926")
  )
})

test_that("Property 4: Jaccard entries are empty when network arrays missing", {
  report <- run_dashboard_in_fixtures(
    include_ch4 = FALSE, include_ch5 = FALSE, include_ch7 = FALSE,
    include_arrays = FALSE, include_quan = FALSE
  )

  expect_true("network_stability" %in% names(report))
  expect_equal(length(report$network_stability), 0)
})


# ===========================================================================
# Unit Tests: Validation Dashboard Edge Cases
# Feature: portfolio-packaging, Task 2.3
# **Validates: Requirements 3.5**
# ===========================================================================

# ---- Edge Case 1: All chapters missing → all "not_run" --------------------

test_that("Edge Case 1: all chapters missing produces all not_run statuses", {
  report <- run_dashboard_in_fixtures(
    include_ch4 = FALSE, include_ch5 = FALSE, include_ch7 = FALSE,
    include_arrays = FALSE, include_quan = FALSE
  )

  expect_true(is.list(report))
  expect_equal(length(report$chapters), 3)

  for (ch in report$chapters) {
    expect_equal(ch$status, "not_run",
                 info = paste(ch$chapter, "should be not_run when no outputs exist"))
  }

  expect_equal(report$overall_status, "not_run")
  expect_false(report$overall_passed,
               info = "a dashboard with no executed chapters is not a passing run")
})


# ---- Edge Case 2: NAM coefficient mismatch → "fail" with expected/actual --

test_that("Edge Case 2: proxy NAM mismatch produces warning with expected vs actual", {
  # Build fixture tree manually so we can inject mismatched NAM values
  base  <- create_fixture_tree()
  repro <- file.path(base, "reproduced")
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  # Write nam_summary.csv with values that DON'T match the thesis expectations
  d <- file.path(repro, "outputs", "chapter5", "tables")
  write.csv(data.frame(
    time_period = c("time1", "time1", "time2", "time2"),
    term        = c("intercept", "network_effect",
                    "intercept", "network_effect"),
    estimate    = c(9.999, 0.111, 8.888, 0.222),
    std_error   = c(0.1, 0.05, 0.12, 0.06),
    stringsAsFactors = FALSE
  ), file.path(d, "nam_summary.csv"), row.names = FALSE)

  # Add quan_results with the "correct" thesis expectations
  add_quan_results(repro)

  # Clone DASH_ENV and point at our fixture tree
  env <- new.env(parent = parent.env(DASH_ENV))
  for (nm in ls(DASH_ENV, all.names = TRUE)) {
    obj <- get(nm, envir = DASH_ENV)
    if (is.function(obj)) environment(obj) <- env
    assign(nm, obj, envir = env)
  }

  env$repro_root            <- repro
  env$repo_root             <- base
  env$ch4_manifests_dir     <- file.path(repro, "outputs", "chapter4", "manifests")
  env$ch4_data_dir          <- file.path(repro, "outputs", "chapter4", "data")
  env$ch5_tables_dir        <- file.path(repro, "outputs", "chapter5", "tables")
  env$ch7_logs_dir          <- file.path(repro, "outputs", "chapter7", "logs")
  env$nam_summary_path      <- file.path(repro, "outputs", "chapter5", "tables",
                                         "nam_summary.csv")
  env$saom_diagnostics_path <- file.path(repro, "outputs", "chapter7", "logs",
                                         "saom_diagnostics.json")
  env$network_arrays_path   <- file.path(repro, "outputs", "chapter4", "data",
                                         "network_arrays.rds")
  env$quan_results_path     <- file.path(repro, "docs", "references",
                                         "quan_results.md")
  env$data_mode             <- "proxy"

  report <- env$build_report()

  # Find Ch.5 result
  ch5 <- NULL
  for (ch in report$chapters) {
    if (ch$chapter == "chapter5_descriptive_norms") { ch5 <- ch; break }
  }
  expect_false(is.null(ch5))
  expect_equal(ch5$status, "warning",
               info = "proxy coefficients are not expected to reproduce empirical thesis values")
  expect_equal(report$overall_status, "warning")
  expect_true(report$overall_passed)

  # Find the nam_coefficients check
  nam_check <- NULL
  for (check in ch5$checks) {
    if (check$name == "nam_coefficients") { nam_check <- check; break }
  }
  expect_false(is.null(nam_check))
  expect_true(length(nam_check$metrics) > 0)

  # Every metric should have expected and actual, and passed should be FALSE
  for (m in nam_check$metrics) {
    expect_true("expected" %in% names(m),
                info = paste("Metric for", m$term, "must have 'expected'"))
    expect_true("actual" %in% names(m),
                info = paste("Metric for", m$term, "must have 'actual'"))
    expect_true(is.numeric(m$expected))
    expect_true(is.numeric(m$actual))
    expect_false(isTRUE(m$passed),
                 info = paste("Metric for", m$term, "should NOT pass with mismatched values"))
    # Verify expected != actual (the mismatch is real)
    expect_false(abs(m$expected - m$actual) < 0.01,
                 info = paste("Expected and actual should differ for", m$term))
  }
})

test_that("Edge Case 2b: real-data NAM mismatch remains a hard failure", {
  base  <- create_fixture_tree()
  repro <- file.path(base, "reproduced")
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  d <- file.path(repro, "outputs", "chapter5", "tables")
  write.csv(data.frame(
    time_period = c("time1", "time1", "time2", "time2"),
    term = c("intercept", "network_effect", "intercept", "network_effect"),
    estimate = c(9.999, 0.111, 8.888, 0.222),
    stringsAsFactors = FALSE
  ), file.path(d, "nam_summary.csv"), row.names = FALSE)
  add_quan_results(repro)

  env <- new.env(parent = parent.env(DASH_ENV))
  for (nm in ls(DASH_ENV, all.names = TRUE)) {
    obj <- get(nm, envir = DASH_ENV)
    if (is.function(obj)) environment(obj) <- env
    assign(nm, obj, envir = env)
  }
  env$repro_root <- repro
  env$repo_root <- base
  env$ch4_manifests_dir <- file.path(repro, "outputs", "chapter4", "manifests")
  env$ch4_data_dir <- file.path(repro, "outputs", "chapter4", "data")
  env$ch5_tables_dir <- d
  env$ch7_logs_dir <- file.path(repro, "outputs", "chapter7", "logs")
  env$nam_summary_path <- file.path(d, "nam_summary.csv")
  env$nam_comparison_path <- file.path(d, "nam_comparison.csv")
  env$saom_diagnostics_path <- file.path(env$ch7_logs_dir, "saom_diagnostics.json")
  env$network_arrays_path <- file.path(env$ch4_data_dir, "network_arrays.rds")
  env$quan_results_path <- file.path(repro, "docs", "references", "quan_results.md")
  env$data_mode <- "real"

  report <- env$build_report()
  ch5 <- Filter(function(ch) ch$chapter == "chapter5_descriptive_norms", report$chapters)[[1]]
  expect_equal(ch5$status, "fail")
  expect_equal(report$overall_status, "fail")
  expect_false(report$overall_passed)
})


# ---- Edge Case 3: Malformed SAOM diagnostics JSON → "fail" status ---------

test_that("Edge Case 3: malformed SAOM diagnostics JSON produces fail status", {
  base  <- create_fixture_tree()
  repro <- file.path(base, "reproduced")
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  # Write invalid JSON to the diagnostics file
  d <- file.path(repro, "outputs", "chapter7", "logs")
  writeLines("{ this is not valid JSON !!!", file.path(d, "saom_diagnostics.json"))

  # Clone DASH_ENV and point at our fixture tree
  env <- new.env(parent = parent.env(DASH_ENV))
  for (nm in ls(DASH_ENV, all.names = TRUE)) {
    obj <- get(nm, envir = DASH_ENV)
    if (is.function(obj)) environment(obj) <- env
    assign(nm, obj, envir = env)
  }

  env$repro_root            <- repro
  env$repo_root             <- base
  env$ch4_manifests_dir     <- file.path(repro, "outputs", "chapter4", "manifests")
  env$ch4_data_dir          <- file.path(repro, "outputs", "chapter4", "data")
  env$ch5_tables_dir        <- file.path(repro, "outputs", "chapter5", "tables")
  env$ch7_logs_dir          <- file.path(repro, "outputs", "chapter7", "logs")
  env$nam_summary_path      <- file.path(repro, "outputs", "chapter5", "tables",
                                         "nam_summary.csv")
  env$saom_diagnostics_path <- file.path(repro, "outputs", "chapter7", "logs",
                                         "saom_diagnostics.json")
  env$network_arrays_path   <- file.path(repro, "outputs", "chapter4", "data",
                                         "network_arrays.rds")
  env$quan_results_path     <- file.path(repro, "docs", "references",
                                         "quan_results.md")
  env$data_mode             <- "proxy"

  report <- env$build_report()

  # Find Ch.7 result
  ch7 <- NULL
  for (ch in report$chapters) {
    if (ch$chapter == "chapter7_saom") { ch7 <- ch; break }
  }
  expect_false(is.null(ch7))
  expect_equal(ch7$status, "fail",
               info = "Ch.7 should be 'fail' when diagnostics JSON is malformed")
})


# ---- Edge Case 4: Rendered Markdown contains failure details ---------------

test_that("Edge Case 4: rendered Markdown contains expected vs actual for failures", {
  # Build fixture tree with mismatched NAM values (same as Edge Case 2)
  base  <- create_fixture_tree()
  repro <- file.path(base, "reproduced")
  on.exit(unlink(base, recursive = TRUE), add = TRUE)

  d <- file.path(repro, "outputs", "chapter5", "tables")
  write.csv(data.frame(
    time_period = c("time1", "time1", "time2", "time2"),
    term        = c("intercept", "network_effect",
                    "intercept", "network_effect"),
    estimate    = c(9.999, 0.111, 8.888, 0.222),
    std_error   = c(0.1, 0.05, 0.12, 0.06),
    stringsAsFactors = FALSE
  ), file.path(d, "nam_summary.csv"), row.names = FALSE)

  add_quan_results(repro)

  # Clone DASH_ENV and point at our fixture tree
  env <- new.env(parent = parent.env(DASH_ENV))
  for (nm in ls(DASH_ENV, all.names = TRUE)) {
    obj <- get(nm, envir = DASH_ENV)
    if (is.function(obj)) environment(obj) <- env
    assign(nm, obj, envir = env)
  }

  env$repro_root            <- repro
  env$repo_root             <- base
  env$ch4_manifests_dir     <- file.path(repro, "outputs", "chapter4", "manifests")
  env$ch4_data_dir          <- file.path(repro, "outputs", "chapter4", "data")
  env$ch5_tables_dir        <- file.path(repro, "outputs", "chapter5", "tables")
  env$ch7_logs_dir          <- file.path(repro, "outputs", "chapter7", "logs")
  env$nam_summary_path      <- file.path(repro, "outputs", "chapter5", "tables",
                                         "nam_summary.csv")
  env$saom_diagnostics_path <- file.path(repro, "outputs", "chapter7", "logs",
                                         "saom_diagnostics.json")
  env$network_arrays_path   <- file.path(repro, "outputs", "chapter4", "data",
                                         "network_arrays.rds")
  env$quan_results_path     <- file.path(repro, "docs", "references",
                                         "quan_results.md")
  env$data_mode             <- "proxy"

  report <- env$build_report()

  # Render to Markdown
  md_lines <- env$render_dashboard(report)
  md_text  <- paste(md_lines, collapse = "\n")

  expect_true(grepl("STRUCTURAL PASS WITH WARNINGS", md_text),
              info = "proxy mismatches should be disclosed without claiming empirical failure")

  # The Markdown should contain both expected and actual values in the table
  # Expected values from quan_results: 1.234, 0.567, 1.345, 0.678
  # Actual values from mismatched CSV: 9.999, 0.111, 8.888, 0.222
  expect_true(grepl("1\\.2340", md_text),
              info = "Rendered Markdown should contain expected value 1.2340")
  expect_true(grepl("9\\.9990", md_text),
              info = "Rendered Markdown should contain actual value 9.9990")
  expect_true(grepl("0\\.5670", md_text),
              info = "Rendered Markdown should contain expected value 0.5670")
  expect_true(grepl("0\\.1110", md_text),
              info = "Rendered Markdown should contain actual value 0.1110")

  expect_true(grepl("\u26a0\ufe0f", md_text),
              info = "Rendered Markdown should flag proxy mismatches as warnings")
})
