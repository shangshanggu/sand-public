#!/usr/bin/env Rscript

activate_renv <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args, fixed = TRUE)])
  script_dir <- NULL
  if (length(script_path)) {
    script_dir <- tryCatch(
      dirname(normalizePath(script_path[1], winslash = "/", mustWork = TRUE)),
      error = function(...) NULL
    )
  }
  candidates <- unique(stats::na.omit(c(
    getwd(),
    if (!is.null(script_dir)) script_dir,
    if (!is.null(script_dir)) normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE) else NULL,
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
  )))
  for (candidate in candidates) {
    if (!is.character(candidate) || !nzchar(candidate)) {
      next
    }
    activate_path <- file.path(candidate, "renv", "activate.R")
    if (file.exists(activate_path)) {
      source(activate_path, local = FALSE)
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

suppressWarnings({
  args <- commandArgs(trailingOnly = TRUE)
})

try(activate_renv(), silent = TRUE)

usage <- function() {
  cat(
    "Usage: install_r_packages.R --packages PKG1,PKG2,... [--check-only {true|false}] [--repos URL] [--prefer-binary {true|false}] [--library PATH] [--ncpus N]",
    "\n",
    sep = ""
  )
}

parse_flag <- function(flag, default = NULL) {
  if (!length(args)) {
    return(default)
  }
  idx <- which(args == flag)
  if (!length(idx)) {
    return(default)
  }
  if (idx == length(args)) {
    stop(sprintf("Flag '%s' requires a value.", flag), call. = FALSE)
  }
  args[[idx + 1L]]
}

bool_from <- function(value, default = TRUE) {
  if (is.null(value)) {
    return(default)
  }
  if (!nzchar(value)) {
    return(default)
  }
  tolower(value) %in% c("1", "true", "yes", "on")
}

packages_arg <- parse_flag("--packages")
if (is.null(packages_arg)) {
  usage()
  stop("--packages must be provided.", call. = FALSE)
}

split_packages <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(character())
  }
  raw <- unlist(strsplit(x, "[,[:space:]]+"))
  unique(raw[nzchar(raw)])
}

packages <- split_packages(packages_arg)
if (!length(packages)) {
  message("No packages specified; nothing to install.")
  quit(status = 0L, save = "no")
}

split_repos <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(character())
  }
  parts <- unlist(strsplit(x, "[;,[:space:]]+"))
  unique(parts[nzchar(parts)])
}

repos_arg <- parse_flag("--repos", "")
repos <- split_repos(repos_arg)
if (!length(repos)) {
  repos <- c(
    "https://packagemanager.posit.co/cran/latest",
    "https://cloud.r-project.org"
  )
}
prefer_binary <- bool_from(parse_flag("--prefer-binary", "false"), default = FALSE)
check_only <- bool_from(parse_flag("--check-only", "false"), default = FALSE)
lib_path <- parse_flag("--library", NULL)
threads <- parse_flag("--ncpus", "")
ncpus <- suppressWarnings(as.integer(threads))
if (is.na(ncpus) || ncpus < 1L) {
  if (requireNamespace("parallel", quietly = TRUE)) {
    ncpus <- max(1L, parallel::detectCores(logical = TRUE))
  } else {
    ncpus <- 1L
  }
}

if (!is.null(lib_path) && nzchar(lib_path)) {
  dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(normalizePath(lib_path, winslash = "/", mustWork = FALSE), .libPaths()))
}

pkg_type <- if (prefer_binary) "binary" else "source"

set_repo_options <- function(repo) {
  options(repos = c(CRAN = repo))
  Sys.setenv(RSPM = repo)
  Sys.setenv(PAK_REPOS_OVERRIDE = repo)
  Sys.setenv(R_DOWNLOAD_FILE_METHOD = "libcurl")
  Sys.setenv(RENV_DOWNLOAD_METHOD = "libcurl")
  options(download.file.method = "libcurl")
}

set_repo_options(repos[[1]])
options(pkgType = pkg_type)
if (prefer_binary) {
  Sys.setenv(PAK_PKG_TYPE = "binary")
} else {
  Sys.unsetenv("PAK_PKG_TYPE")
}

is_missing <- !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
missing_pkgs <- packages[is_missing]
if (!length(missing_pkgs)) {
  message("All requested packages are already installed.")
  quit(status = 0L, save = "no")
}

if (check_only) {
  stop(
    sprintf(
      "Required packages are missing from the locked environment: %s. Run 'make env-renv' before the pipeline.",
      paste(missing_pkgs, collapse = ", ")
    ),
    call. = FALSE
  )
}

message(sprintf("Installing %d package(s): %s", length(missing_pkgs), paste(missing_pkgs, collapse = ", ")))

install_with_pak <- function(pkgs, repo) {
  tryCatch({
    force(repo)
    if (!requireNamespace("pak", quietly = TRUE)) {
      pak_repo <- sprintf(
        "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
        .Platform$pkgType,
        R.Version()$os,
        R.Version()$arch
      )
      install.packages("pak", repos = pak_repo, dependencies = FALSE, Ncpus = ncpus, type = "source")
    }
    suppressMessages(
      pak::pkg_install(pkgs, ask = FALSE, upgrade = TRUE, dependencies = TRUE, lib = .libPaths()[1L])
    )
    TRUE
  }, error = function(e) {
    message(sprintf("pak installation failed: %s", conditionMessage(e)))
    FALSE
  })
}

install_with_base <- function(pkgs, repo, pkg_type) {
  tryCatch({
    install.packages(
      pkgs,
      repos = repo,
      dependencies = c("Depends", "Imports", "LinkingTo"),
      Ncpus = ncpus,
      type = pkg_type
    )
    still_missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still_missing)) {
      message(sprintf(
        "install.packages completed but missing packages remain: %s",
        paste(still_missing, collapse = ", ")
      ))
      return(FALSE)
    }
    TRUE
  }, error = function(e) {
    message(sprintf("install.packages fallback failed: %s", conditionMessage(e)))
    FALSE
  })
}

max_attempts <- max(1L, as.integer(parse_flag("--retries", "3")))

attempt_install <- function(repo) {
  set_repo_options(repo)
  base_pkg_type <- pkg_type
  if (prefer_binary) {
    if (install_with_pak(missing_pkgs, repo)) {
      return(TRUE)
    }
    base_pkg_type <- "source"
  }
  install_with_base(missing_pkgs, repo, base_pkg_type)
}

attempts <- 0L
success <- FALSE
last_repo <- NA_character_
for (repo in repos) {
  for (i in seq_len(max_attempts)) {
    attempts <- attempts + 1L
    last_repo <- repo
    message(sprintf("Attempt %d: installing from %s", attempts, repo))
    if (attempt_install(repo)) {
      success <- TRUE
      break
    }
    if (i < max_attempts) {
      delay <- min(60, 2 ^ (i - 1))
      message(sprintf("Retrying in %d second(s)...", delay))
      Sys.sleep(delay)
    }
  }
  if (success) {
    break
  }
}

if (!success) {
  stop(
    sprintf(
      "Failed to install requested packages after %d attempt(s). Last tried repo: %s. Missing: %s",
      attempts,
      last_repo,
      paste(missing_pkgs, collapse = ", ")
    ),
    call. = FALSE
  )
}

message("Package installation completed.")
quit(status = 0L, save = "no")
