#!/usr/bin/env Rscript

required_packages <- c(
  "broom", "cowplot", "dplyr", "ggplot2", "gridExtra", "igraph",
  "purrr", "readr", "RSiena", "stringr", "tidyr", "yaml"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_packages) > 0) {
  stop(
    sprintf("[smoke] Missing required packages: %s", paste(missing_packages, collapse = ", ")),
    call. = FALSE
  )
}

cat(sprintf("[smoke] Loaded %d required R packages successfully.\n", length(required_packages)))

project_root <- if (dir.exists("reproduced")) "reproduced" else "."
raw_paths <- file.path(project_root, "data", "raw", c("participants.csv", "outcomes.csv"))

missing <- raw_paths[!file.exists(raw_paths)]

if (length(missing) > 0) {
  cat(
    sprintf(
      "[smoke] Protected raw data are not staged (%s); this is expected for the public image. Use explicit proxy mode for public workflow checks.\n",
      paste(missing, collapse = ", ")
    )
  )
} else {
  cat("[smoke] Raw data files detected.\n")
}

cat("[smoke] Environment smoke test completed.\n")
