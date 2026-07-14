#!/usr/bin/env Rscript

# Shared utilities for chapter scripts: deterministic RNG configuration,
# standardised JSON logging, and directory helpers.

ensure_jsonlite <- function(context = NULL) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    return(invisible(TRUE))
  }

  hint <- "Install it by running `make env-renv` or `R -e \"install.packages('jsonlite')\"`."
  detail <- if (!is.null(context) && nzchar(context)) paste0("\nContext: ", context) else ""
  stop(paste0("The jsonlite package is required for logging utilities. ", hint, detail))
}

#' Set the global RNG state to a reproducible configuration.
#'
#' @param seed Numeric scalar used to initialise the RNG.
#' @param normal_kind RNG setting used for normal deviates.
#' @param sample_kind RNG setting used for sampling routines.
#' @return Invisibly returns the integer seed applied.
set_deterministic_seed <- function(seed,
                                   normal_kind = "Inversion",
                                   sample_kind = "Rejection") {
  if (missing(seed) || is.null(seed) || length(seed) != 1L || is.na(seed)) {
    stop("`seed` must be a single non-missing numeric value.")
  }
  if (!is.numeric(seed)) {
    stop("`seed` must be numeric.")
  }
  seed <- as.integer(seed)
  tryCatch(
    {
      RNGkind(kind = "Mersenne-Twister", normal.kind = normal_kind, sample.kind = sample_kind)
    },
    warning = function(w) {
      RNGkind(kind = "Mersenne-Twister", normal.kind = normal_kind)
    },
    error = function(e) {
      RNGkind(kind = "Mersenne-Twister")
    }
  )
  set.seed(seed)
  invisible(seed)
}

#' Format a POSIXct timestamp using the pipeline's canonical representation.
#'
#' @param time POSIXt or coercible value.
#' @param tz Timezone for output, defaults to UTC.
#' @return ISO-8601 formatted timestamp string.
format_timestamp <- function(time = Sys.time(), tz = "UTC") {
  if (is.null(time)) {
    return(NULL)
  }
  posix <- as.POSIXct(time, tz = tz)
  formatted <- format(posix, "%Y-%m-%dT%H:%M:%OS", tz = tz, usetz = FALSE)
  sub("\\.0+", "", paste0(formatted, "Z"))
}

#' Capture timing metadata for a long-running computation.
#'
#' @param tz Timezone used when formatting timestamps; defaults to UTC.
#' @return A list containing the start time, timezone, RNG seed, and a helper to
#'         finalise the capture when the run completes.
create_run_context <- function(tz = "UTC") {
  start_time <- Sys.time()
  rng_seed <- tryCatch(
    {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        as.integer(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      } else {
        NULL
      }
    },
    error = function(...) NULL
  )
  list(
    start_time = start_time,
    timezone = tz,
    rng_seed = rng_seed
  )
}

#' Finalise a run context created by `create_run_context()`.
#'
#' @param context A list returned by `create_run_context()`.
#' @param end_time Optional POSIXct timestamp to treat as the finish time.
#' @return A list with formatted timestamps, duration (seconds), and the
#'         captured RNG seed.
complete_run_context <- function(context, end_time = Sys.time()) {
  if (is.null(context) || !is.list(context)) {
    stop("`context` must be the object returned by create_run_context().")
  }

  tz <- context$timezone
  if (is.null(tz) || !nzchar(tz)) {
    tz <- "UTC"
  }

  start_time <- context$start_time
  end_time <- as.POSIXct(end_time, tz = tz)

  started_at <- if (!is.null(start_time)) {
    format_timestamp(start_time, tz = tz)
  } else {
    NULL
  }

  duration_seconds <- if (!is.null(start_time)) {
    as.numeric(difftime(end_time, as.POSIXct(start_time, tz = tz), units = "secs"))
  } else {
    NULL
  }

  list(
    started_at = started_at,
    finished_at = format_timestamp(end_time, tz = tz),
    duration_seconds = duration_seconds,
    rng_seed = context$rng_seed
  )
}

#' Convert an absolute path into a repo-relative path when possible.
#'
#' @param path File or directory path to relativise.
#' @param repo_root Repository root directory.
#' @return Relative path string or the original path if it is outside the repo.
relative_repo_path <- function(path, repo_root) {
  if (is.null(path) || !nzchar(path)) {
    return(path)
  }
  repo_norm <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (identical(path_norm, repo_norm)) {
    return(".")
  }
  prefix <- paste0(repo_norm, "/")
  if (startsWith(path_norm, prefix)) {
    return(substring(path_norm, nchar(prefix) + 1L))
  }
  path_norm
}

#' Ensure the parent directory for a file exists.
#'
#' @param file_path Target file path.
#' @return Invisibly returns the directory path that was created or already existed.
ensure_parent_dir <- function(file_path) {
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(dir_path)
}

#' Append a run entry to a JSON history log.
#'
#' @param log_path Destination JSON file.
#' @param entry Named list representing the run metadata.
#' @param history_key Name of the list element that stores the history array.
#' @return Invisibly returns the log path.
append_run_log <- function(log_path, entry, history_key = "history") {
  ensure_parent_dir(log_path)
  ensure_jsonlite("Needed to serialise run history entries.")
  existing <- list()
  if (file.exists(log_path)) {
    existing <- tryCatch(
      jsonlite::read_json(log_path, simplifyVector = FALSE),
      error = function(e) list()
    )
  }
  history <- existing[[history_key]]
  if (is.null(history) || !is.list(history)) {
    history <- list()
  }
  history <- c(history, list(entry))
  existing[[history_key]] <- history
  jsonlite::write_json(existing, log_path, auto_unbox = TRUE, pretty = TRUE)
  invisible(log_path)
}

#' Append a run entry to a pipeline-level log under reproduced/logs/.
#'
#' @param logs_root Base directory for pipeline logs.
#' @param name Subdirectory name (e.g., "chapter5").
#' @param entry Run entry payload.
#' @param history_key Name of the history field (defaults to "history").
#' @return Invisibly returns the path to the pipeline log file.
append_pipeline_log <- function(logs_root, name, entry, history_key = "history") {
  if (is.null(logs_root) || !nzchar(logs_root)) {
    stop("`logs_root` must be provided for pipeline logging.")
  }
  if (is.null(name) || !nzchar(name)) {
    stop("`name` must be provided for pipeline logging.")
  }
  log_path <- file.path(logs_root, name, "run.json")
  append_run_log(log_path, entry, history_key = history_key)
}

#' Helper to relativise all character elements in a list.
#'
#' @param x Arbitrary list/atomic vector of paths.
#' @param repo_root Repository root directory.
#' @return Structure with character vectors converted to repo-relative paths.
relativise_paths <- function(x, repo_root) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.character(x)) {
    return(vapply(x, relative_repo_path, character(1), repo_root = repo_root))
  }
  if (is.list(x)) {
    return(lapply(x, relativise_paths, repo_root = repo_root))
  }
  x
}

#' Capture the current RNG state as an integer vector for logging.
#'
#' @param envir Environment to inspect for `.Random.seed`.
#' @return Integer vector representing the RNG state, or `NULL` if unset.
capture_rng_state <- function(envir = .GlobalEnv) {
  if (!exists(".Random.seed", envir = envir, inherits = FALSE)) {
    return(NULL)
  }
  seed <- get(".Random.seed", envir = envir, inherits = FALSE)
  if (is.null(seed)) {
    return(NULL)
  }
  as.integer(seed)
}

#' Extract the effective seed value from an RNG state vector.
#'
#' @param state Integer vector as returned by `capture_rng_state()`.
#' @return Integer scalar seed, or `NULL` if unavailable.
extract_seed_from_state <- function(state) {
  if (is.null(state) || length(state) < 2) {
    return(NULL)
  }
  as.integer(state[2])
}

#' Begin a run metadata context capturing start time and RNG information.
#'
#' @param tz Timezone identifier used when later formatting timestamps.
#' @param capture_rng Logical indicating whether to capture the RNG state.
#' @return A list storing raw metadata for later completion.
begin_run_metadata <- function(tz = "UTC", capture_rng = TRUE) {
  start_time <- Sys.time()
  rng_state <- if (capture_rng) capture_rng_state() else NULL
  list(
    started_at_raw = start_time,
    tz = tz,
    rng_state = rng_state,
    rng_seed = extract_seed_from_state(rng_state)
  )
}

#' Finalise a run metadata context with end time and duration.
#'
#' @param context Context list produced by `begin_run_metadata()`.
#' @param status Optional status label to include with the metadata.
#' @return A list ready for JSON serialisation including timing metrics.
finalise_run_metadata <- function(context, status = NULL) {
  if (is.null(context$started_at_raw)) {
    return(list(status = status))
  }
  end_time <- Sys.time()
  tz <- context$tz
  if (is.null(tz) || !nzchar(tz)) {
    tz <- "UTC"
  }

  started_at <- context$started_at_raw
  metadata <- list(
    started_at = format_timestamp(started_at, tz = tz),
    finished_at = format_timestamp(end_time, tz = tz),
    duration_seconds = as.numeric(difftime(end_time, started_at, units = "secs"))
  )

  if (!is.null(context$rng_seed)) {
    metadata$rng_seed <- context$rng_seed
  }
  if (!is.null(context$rng_state)) {
    metadata$rng_state <- context$rng_state
  }
  if (!is.null(status)) {
    metadata$status <- status
  }

  metadata
}

#' Safely read an RDS file, returning a default value on failure.
#'
#' @param path Path to the RDS file.
#' @param default Value to return if the file cannot be read.
#' @param warn Logical flag controlling whether to emit warnings on failure.
#' @return Parsed object or `default` when the file is missing/invalid.
safe_read_rds <- function(path, default = NULL, warn = TRUE) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(default)
  }

  tryCatch(
    readRDS(path),
    error = function(err) {
      if (warn) {
        message(sprintf("Failed to read RDS from %s: %s", path, conditionMessage(err)))
      }
      default
    }
  )
}

#' Safely write an object to an RDS file, creating parent directories as needed.
#'
#' @param object Object to serialise.
#' @param path Destination RDS path.
#' @param silent Logical flag controlling whether to suppress failure messages.
#' @return Invisibly returns `TRUE` on success, `FALSE` otherwise.
safe_write_rds <- function(object, path, silent = FALSE) {
  if (is.null(path) || !nzchar(path)) {
    stop("`path` must be provided when writing an RDS file.")
  }

  ensure_parent_dir(path)
  tryCatch(
    {
      saveRDS(object, path)
      invisible(TRUE)
    },
    error = function(err) {
      if (!silent) {
        message(sprintf("Failed to write RDS to %s: %s", path, conditionMessage(err)))
      }
      invisible(FALSE)
    }
  )
}
