# ---------------------------------------------------------------------------
# Property tests for the Privacy Guard (Property 12)
#
# Feature: portfolio-packaging
# Validates: Requirements 7.3
# ---------------------------------------------------------------------------

library(testthat)

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
PRIVACY_SCRIPT <- file.path(REPRO_ROOT, "scripts", "portfolio", "check_privacy.R")
stopifnot(file.exists(PRIVACY_SCRIPT))

.load_privacy_env <- function() {
  env <- new.env(parent = globalenv())
  source(PRIVACY_SCRIPT, local = env)
  env
}

PRIVACY_ENV <- .load_privacy_env()

`%||%` <- function(x, y) if (!is.null(x)) x else y


# ===========================================================================
# Property 12: Privacy guard rejects files containing real identifiers
# Feature: portfolio-packaging, Property 12: privacy guard rejection
# **Validates: Requirements 7.3**
# ===========================================================================

test_that("Property 12: safe synthetic outputs pass privacy scan", {
  td <- tempfile(pattern = "privacy_safe_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  writeLines(
    c(
      "# Example Output",
      "Generated: 2026-02-12T12:00:00Z",
      "Synthetic actor P001 has proxy AUDIT-C score 3."
    ),
    file.path(td, "safe.md")
  )

  result <- PRIVACY_ENV$scan_directory(td)
  expect_equal(length(result$files), 1L)
  expect_equal(length(result$violations), 0L)
  expect_true(isTRUE(PRIVACY_ENV$run_privacy_check(td)))
})

test_that("Property 12: network HTML requires visible and machine-readable proxy provenance", {
  td <- tempfile(pattern = "privacy_provenance_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  network_path <- file.path(td, "network_wave2.html")
  writeLines("<html><body>Network</body></html>", network_path)
  result <- PRIVACY_ENV$scan_directory(td)
  expect_true(any(vapply(
    result$violations,
    function(v) identical(v$kind, "missing synthetic proxy provenance"),
    logical(1)
  )))
  expect_false(isTRUE(PRIVACY_ENV$run_privacy_check(td)))

  writeLines(
    c(
      "<html><body>",
      "<div data-sand-data-mode='proxy'><strong>SYNTHETIC PROXY DATA</strong></div>",
      "</body></html>"
    ),
    network_path
  )
  expect_true(isTRUE(PRIVACY_ENV$run_privacy_check(td)))
})

test_that("Property 12: numeric participant identifiers are detected with file+line", {
  td <- tempfile(pattern = "privacy_fail_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  file_path <- file.path(td, "leak.md")
  writeLines(
    c(
      "Participant 1234 has score 5.",
      "No issue here."
    ),
    file_path
  )

  result <- PRIVACY_ENV$scan_directory(td)
  expect_true(length(result$violations) >= 1L)

  first <- result$violations[[1]]
  expect_equal(normalizePath(first$file), normalizePath(file_path))
  expect_equal(first$line, 1L)
  expect_true("1234" %in% first$ids)
  expect_false(isTRUE(PRIVACY_ENV$run_privacy_check(td)))
})

test_that("Property 12: CLI exits non-zero when real identifiers are present", {
  td <- tempfile(pattern = "privacy_cli_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  writeLines(
    c(
      "{\"redcap_survey_identifier\": 4567}",
      "Participant P001"
    ),
    file.path(td, "leak.html")
  )

  rscript_bin <- file.path(R.home("bin"), "Rscript")
  out <- suppressWarnings(
    system2(
      rscript_bin,
      args = c(PRIVACY_SCRIPT, td),
      stdout = TRUE,
      stderr = TRUE
    )
  )
  status <- as.integer(attr(out, "status") %||% 0L)
  expect_equal(status, 1L)
})
