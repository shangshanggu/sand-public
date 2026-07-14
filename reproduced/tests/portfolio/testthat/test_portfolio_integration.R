# ---------------------------------------------------------------------------
# Integration test for full portfolio generation (Task 12.2)
#
# Feature: portfolio-packaging
# Validates: Requirements 7.1, 7.3, 3.6, 5.5
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

run_command_checked <- function(command, args, wd, env = character(0)) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(wd)
  out <- suppressWarnings(system2(
    command,
    args = args,
    stdout = TRUE,
    stderr = TRUE,
    env = env,
    wait = TRUE
  ))
  status <- as.integer(attr(out, "status") %||% 0L)
  list(status = status, output = out)
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

test_that("Integration: proxy-mode make portfolio generates expected outputs and passes privacy", {
  make_bin <- Sys.which("make")
  expect_true(nzchar(make_bin), info = "make command must be available")

  build <- run_command_checked(
    command = make_bin,
    args = c("portfolio"),
    wd = REPRO_ROOT,
    env = c("SAND_DATA_MODE=proxy")
  )
  if (build$status != 0L) {
    fail(paste(
      "make portfolio failed with status", build$status,
      "\nOutput tail:\n",
      paste(tail(build$output, 40), collapse = "\n")
    ))
  }

  expected_files <- c(
    "outputs/portfolio/validation_dashboard.md",
    "outputs/portfolio/data_dictionary.md",
    "outputs/portfolio/reproducibility_manifest.json",
    "outputs/chapter7/interactive_networks/network_wave2.html",
    "outputs/chapter7/interactive_networks/network_wave4.html",
    "outputs/chapter7/interactive_networks/network_wave5.html",
    "outputs/chapter7/interactive_networks/network_wave6.html",
    "outputs/chapter7/interactive_networks/network_all_waves.html"
  )

  for (rel in expected_files) {
    expect_true(
      file.exists(file.path(REPRO_ROOT, rel)),
      info = paste("Expected output missing:", rel)
    )
  }

  privacy <- run_command_checked(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("scripts/portfolio/check_privacy.R", "outputs/portfolio"),
    wd = REPRO_ROOT
  )
  expect_equal(
    privacy$status, 0L,
    info = paste(
      "Privacy guard failed:\n",
      paste(tail(privacy$output, 30), collapse = "\n")
    )
  )

  dashboard_path <- file.path(REPRO_ROOT, "outputs", "portfolio", "validation_dashboard.md")
  lines <- readLines(dashboard_path, warn = FALSE)
  expect_true(length(lines) > 5L)
  expect_true(grepl("^# Validation Dashboard", lines[1]))
  expect_true(any(grepl("^## Pipeline Overview$", lines)))
})
