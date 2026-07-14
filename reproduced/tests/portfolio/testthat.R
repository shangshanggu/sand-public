#!/usr/bin/env Rscript
# Portfolio test runner — executes all tests under testthat/
# Some CI and sandbox environments force LC_ALL. testthat temporarily changes
# message language around expectations; an exported LC_ALL makes R emit one
# warning per expectation. Let the category-specific locale variables inherit
# from LANG instead.
Sys.unsetenv("LC_ALL")
library(testthat)

# Resolve test directory relative to this script's location
get_script_dir <- function() {
  # Try commandArgs approach (works when run via Rscript)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  # Fallback: current working directory
  return(getwd())
}

script_dir <- get_script_dir()
test_path  <- file.path(script_dir, "testthat")

# List test files (test_*.R pattern)
test_files <- list.files(test_path, pattern = "^test.*\\.R$", full.names = TRUE)

if (length(test_files) == 0) {
  message("No test files found in ", test_path, " -- scaffolding OK (0 tests).")
} else {
  test_dir(test_path, reporter = "summary")
}
