# Shared helper for portfolio tests
# Loaded automatically by testthat before each test file

library(testthat)
library(yaml)
library(jsonlite)

# Resolve REPO_ROOT to the reproduced/ directory
.get_helper_dir <- function() {
  # When sourced by testthat, sys.frame(1)$ofile is set
  for (i in seq_len(sys.nframe())) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(ofile) && grepl("helper\\.R$", ofile)) {
      return(dirname(normalizePath(ofile, mustWork = FALSE)))
    }
  }
  # Fallback: assume working directory is reproduced/tests/portfolio
  return(file.path(getwd(), "testthat"))
}

# helper.R lives at reproduced/tests/portfolio/testthat/helper.R
# so we go up 3 levels to reach reproduced/
REPO_ROOT <- normalizePath(file.path(.get_helper_dir(), "..", "..", ".."), mustWork = FALSE)

CONFIG_PATH    <- file.path(REPO_ROOT, "config", "thesis.yml")
PROXY_DATA_DIR <- file.path(REPO_ROOT, "data", "proxy")

# Source shared utilities if available
common_path <- file.path(REPO_ROOT, "R", "common.R")
if (file.exists(common_path)) {
  source(common_path)
}
