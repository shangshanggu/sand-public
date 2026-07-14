# ---------------------------------------------------------------------------
# Property tests for portfolio narrative word limits (Properties 14-15)
#
# Feature: portfolio-packaging
# Validates: Requirements 2.6, 6.5
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

count_words <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # Drop fenced code blocks and inline markup noise to approximate prose words.
  text <- gsub("```[\\s\\S]*?```", " ", text, perl = TRUE)
  text <- gsub("`[^`]*`", " ", text, perl = TRUE)
  text <- gsub("\\[[^\\]]*\\]\\([^\\)]*\\)", " ", text, perl = TRUE)
  tokens <- unlist(strsplit(text, "[^A-Za-z0-9']+", perl = TRUE))
  tokens <- tokens[nzchar(tokens)]
  length(tokens)
}


# ===========================================================================
# Property 14: Case study respects word count limit (<=1500)
# ===========================================================================

test_that("Property 14: case study is <= 1500 words", {
  path <- file.path(REPRO_ROOT, "docs", "portfolio", "case_study.md")
  expect_true(file.exists(path), info = "case_study.md must exist")
  expect_true(count_words(path) <= 1500L)
})


# ===========================================================================
# Property 15: Findings summary respects word count limit (<=800)
# ===========================================================================

test_that("Property 15: findings summary is <= 800 words", {
  path <- file.path(REPRO_ROOT, "docs", "portfolio", "findings_summary.md")
  expect_true(file.exists(path), info = "findings_summary.md must exist")
  expect_true(count_words(path) <= 800L)
})

