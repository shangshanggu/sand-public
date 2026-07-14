#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (!length(args)) {
  message("Usage: Rscript reproduced/scripts/utils/check_r_syntax.R <file1> [file2 ...]")
  quit(status = 1L)
}

had_error <- FALSE

for (path in args) {
  if (!file.exists(path)) {
    message(sprintf("[check_r_syntax] File not found: %s", path))
    had_error <- TRUE
    next
  }

  message(sprintf("[check_r_syntax] Parsing %s", path))
  parse_result <- tryCatch(
    {
      parse(file = path, keep.source = TRUE)
      TRUE
    },
    error = function(err) {
      message(sprintf("[check_r_syntax] Syntax error in %s: %s", path, conditionMessage(err)))
      FALSE
    }
  )

  if (!parse_result) {
    had_error <- TRUE
  }
}

if (had_error) {
  quit(status = 1L)
}

message("[check_r_syntax] All files parsed successfully.")
quit(status = 0L)
