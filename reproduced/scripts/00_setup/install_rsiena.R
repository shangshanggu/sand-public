#!/usr/bin/env Rscript

default_repo <- "https://packagemanager.posit.co/cran/latest"

ensure_dependency <- function(pkg, repo = default_repo) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(TRUE))
  }

  tryCatch({
    utils::install.packages(pkg, repos = repo, quiet = TRUE)
  }, error = function(err) {
    message(sprintf("[rsiena] Failed to install dependency %s: %s", pkg, err$message))
  })

  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("The '%s' package is required. Install it via install.packages('%s').", pkg, pkg), call. = FALSE)
  }
  invisible(TRUE)
}

ensure_dependency("yaml")
ensure_dependency("jsonlite")
ensure_dependency("digest")

suppressWarnings(suppressMessages({
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The 'yaml' package is required. Install it via install.packages('yaml').", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required. Install it via install.packages('jsonlite').", call. = FALSE)
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The 'digest' package is required. Install it via install.packages('digest').", call. = FALSE)
  }
}))

library(jsonlite)
library(yaml)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (is.character(x) && length(x) == 1 && !nzchar(x)) {
    return(y)
  }
  x
}

with_sanitized_makevars <- function(code) {
  code <- substitute(code)

  old_makevars_user <- Sys.getenv("R_MAKEVARS_USER", unset = NA_character_)
  old_makevars_site <- Sys.getenv("R_MAKEVARS_SITE", unset = NA_character_)

  sanitized_makevars <- tempfile("makevars-user-")
  writeLines(character(0), sanitized_makevars, useBytes = TRUE)

  Sys.setenv(R_MAKEVARS_USER = sanitized_makevars)
  Sys.unsetenv("R_MAKEVARS_SITE")

  on.exit({
    if (!is.na(old_makevars_user) && nzchar(old_makevars_user)) {
      Sys.setenv(R_MAKEVARS_USER = old_makevars_user)
    } else {
      Sys.unsetenv("R_MAKEVARS_USER")
    }
    if (!is.na(old_makevars_site) && nzchar(old_makevars_site)) {
      Sys.setenv(R_MAKEVARS_SITE = old_makevars_site)
    }
  }, add = TRUE)
  on.exit(unlink(sanitized_makevars), add = TRUE)

  eval(code, envir = parent.frame())
}

get_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  start_dir <- if (length(script_path) > 0) {
    normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }

  sentinel_paths <- list(
    list(rel = file.path("config", "thesis.yml"), root_adjust = "."),
    list(rel = file.path("reproduced", "config", "thesis.yml"), root_adjust = ".")
  )
  current <- start_dir
  repeat {
    for (s in sentinel_paths) {
      candidate <- file.path(current, s$rel)
      if (file.exists(candidate)) {
        root_candidate <- normalizePath(file.path(current, s$root_adjust), winslash = "/", mustWork = TRUE)
        # If we matched inside the reproduced/ subtree, lift root to the parent repo directory.
        if (basename(root_candidate) == "reproduced") {
          root_candidate <- normalizePath(file.path(root_candidate, ".."), winslash = "/", mustWork = TRUE)
        }
        return(root_candidate)
      }
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Unable to locate repository root containing config/thesis.yml")
    }
    current <- parent
  }
}

ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

resolve_repo_path <- function(repo_root, path) {
  normalizePath(file.path(repo_root, path), winslash = "/", mustWork = FALSE)
}

collect_package_status <- function(version_target, library_dir = NULL) {
  package_rows <- tryCatch(
    utils::installed.packages(
      lib.loc = if (is.null(library_dir)) .libPaths() else library_dir,
      noCache = TRUE
    ),
    error = function(err) NULL
  )
  if (is.null(package_rows) || !"RSiena" %in% rownames(package_rows)) {
    return(list(installed = FALSE, version = NULL, version_matches = FALSE, needs_install = TRUE))
  }

  installed_version <- unname(package_rows["RSiena", "Version"])

  version_matches <- TRUE
  if (!is.null(version_target) && !is.null(installed_version)) {
    version_matches <- utils::compareVersion(installed_version, version_target) == 0
  }

  list(
    installed = TRUE,
    version = installed_version,
    version_matches = version_matches,
    needs_install = !version_matches
  )
}

install_from_tarball <- function(tarball_path, version_target, library_dir = NULL) {
  if (is.null(tarball_path) || !nzchar(tarball_path) || !file.exists(tarball_path)) {
    return(FALSE)
  }

  message(sprintf(
    "[rsiena] Installing RSiena %s from source tarball %s",
    version_target %||% "",
    tarball_path
  ))

  tryCatch(with_sanitized_makevars({
    utils::install.packages(
      tarball_path,
      repos = NULL,
      type = "source",
      quiet = FALSE,
      lib = library_dir,
      dependencies = FALSE
    )
    TRUE
  }), error = function(err) {
    message("[rsiena] Source installation failed: ", err$message)
    FALSE
  })
}

attempt_local_source_install <- function(source_url, download_dir, version_target, library_dir, repo_root = NULL) {
  if (is.null(source_url) || !nzchar(source_url)) {
    return(FALSE)
  }

  if (!grepl("^https?://", source_url, ignore.case = TRUE)) {
    candidate <- source_url
    if (!grepl("^/", candidate) && !is.null(repo_root)) {
      candidate <- resolve_repo_path(repo_root, candidate)
    } else {
      candidate <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    }

    if (file.exists(candidate)) {
      message(sprintf("[rsiena] Found local RSiena source tarball at %s", candidate))
      return(install_from_tarball(candidate, version_target, library_dir))
    }
  }

  ensure_directory(download_dir)
  dest_path <- file.path(download_dir, basename(source_url))
  if (file.exists(dest_path)) {
    message(sprintf("[rsiena] Found cached RSiena source tarball at %s", dest_path))
    return(install_from_tarball(dest_path, version_target, library_dir))
  }

  FALSE
}

attempt_install <- function(repo, version_target, install_args, library_dir = NULL) {
  message(sprintf("[rsiena] Installing RSiena %s from %s", version_target %||% "(latest)", repo))
  tryCatch(with_sanitized_makevars({
    utils::install.packages(
      "RSiena",
      repos = repo,
      dependencies = TRUE,
      quiet = FALSE,
      lib = library_dir
    )
    TRUE
  }), error = function(err) {
    message("[rsiena] install.packages from CRAN failed: ", err$message)
    FALSE
  })
}

attempt_download_source_install <- function(source_url, download_dir, version_target, library_dir = NULL) {
  if (is.null(source_url) || !nzchar(source_url)) {
    return(FALSE)
  }

  if (!grepl("^https?://", source_url, ignore.case = TRUE)) {
    return(FALSE)
  }

  ensure_directory(download_dir)
  file_name <- basename(source_url)
  dest_path <- file.path(download_dir, file_name)

  message(sprintf("[rsiena] Downloading RSiena source from %s", source_url))

  tryCatch({
    utils::download.file(source_url, destfile = dest_path, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(err) {
    message("[rsiena] Unable to download RSiena source: ", err$message)
    FALSE
  }) -> downloaded

  if (!downloaded || !file.exists(dest_path)) {
    return(FALSE)
  }
  install_from_tarball(dest_path, version_target, library_dir)
}

write_status_log <- function(log_path, status) {
  if (is.null(log_path) || !nzchar(log_path)) {
    return(invisible(NULL))
  }

  tryCatch({
    jsonlite::write_json(status, log_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
    message("[rsiena] Installation status written to ", log_path)
  }, error = function(err) {
    message("[rsiena] Failed to write installation status: ", err$message)
  })
}

main <- function() {
  repo_root <- get_repo_root()
  config <- yaml::read_yaml(file.path(repo_root, "reproduced", "config", "thesis.yml"))
  rsiena_cfg <- config$rsiena %||% list()
  install_cfg <- rsiena_cfg$installation %||% list()

  version_target <- rsiena_cfg$package_version %||% install_cfg$version
  repo_url <- install_cfg$cran_repo %||% "https://packagemanager.posit.co/cran/latest"
  source_url <- install_cfg$source_url %||% rsiena_cfg$source_url
  download_dir <- resolve_repo_path(repo_root, install_cfg$download_dir %||% "data/vendor")
  library_dir_raw <- install_cfg$library_dir %||% rsiena_cfg$library_dir
  library_dir <- NULL
  if (!is.null(library_dir_raw) && nzchar(library_dir_raw)) {
    library_dir <- resolve_repo_path(repo_root, library_dir_raw)
    ensure_directory(library_dir)
    .libPaths(c(library_dir, .libPaths()))
  }
  log_path <- resolve_repo_path(repo_root, install_cfg$log_path %||% file.path("outputs", "chapter7", "logs", "rsiena_install_status.json"))

  ensure_directory(dirname(log_path))

  status <- collect_package_status(version_target, library_dir)
  status$attempted <- list()
  status$timestamp <- format(Sys.time(), tz = config$project$timezone %||% "UTC")
  status$version_target <- version_target
  status$library_dir <- library_dir_raw %||% NULL

  if (!status$installed || status$needs_install) {
    local_success <- attempt_local_source_install(source_url, download_dir, version_target, library_dir, repo_root = repo_root)
    status$attempted <- c(status$attempted, list(list(method = "local_source", success = local_success, download_dir = download_dir)))

    source_success <- FALSE
    if (!local_success && !is.null(source_url)) {
      source_success <- attempt_download_source_install(source_url, download_dir, version_target, library_dir = library_dir)
      status$attempted <- c(status$attempted, list(list(method = "download_source", success = source_success, url = source_url)))
    }

    cran_success <- FALSE
    if (!local_success && !source_success && (is.null(version_target) || !nzchar(version_target))) {
      cran_success <- attempt_install(repo_url, version_target, install_cfg, library_dir = library_dir)
      status$attempted <- c(status$attempted, list(list(method = "cran", success = cran_success, repo = repo_url)))
    }

    status <- modifyList(status, collect_package_status(version_target, library_dir))
  }

  status$installed_after <- status$installed
  status$version_after <- status$version

  write_status_log(log_path, status)

  if (!status$installed || !isTRUE(status$version_matches)) {
    offline_hint <- ""
    if (!is.null(source_url) && nzchar(source_url)) {
      expected_source <- if (grepl("^https?://", source_url, ignore.case = TRUE)) {
        file.path(download_dir, basename(source_url))
      } else {
        resolve_repo_path(repo_root, source_url)
      }
      offline_hint <- sprintf(" (Offline install: place RSiena source tarball at %s and re-run.)", expected_source)
    }
    stop(
      paste0(
        "The exact configured RSiena version ", version_target,
        " is not installed (found ", status$version %||% "none", "). See the installation log for details.",
        offline_hint
      ),
      call. = FALSE
    )
  }
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(err) {
      message("[rsiena] Installation script failed: ", err$message)
      quit(status = 1)
    }
  )
}
