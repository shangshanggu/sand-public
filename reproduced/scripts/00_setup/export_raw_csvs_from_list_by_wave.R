#!/usr/bin/env Rscript

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- grep(file_arg, cmd_args)
  if (length(matches) > 0) {
    return(normalizePath(sub(file_arg, "", cmd_args[matches[1]]), winslash = "/", mustWork = TRUE))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE))
  }
  stop("Unable to determine script path for export_raw_csvs_from_list_by_wave.")
}

resolve_cli_path <- function(path, repo_root) {
  if (is.null(path) || !nzchar(path)) {
    return(NULL)
  }
  if (grepl("^/|^[A-Za-z]:[/\\\\]", path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(repo_root, path), winslash = "/", mustWork = FALSE)
}

parse_arguments <- function(args) {
  config <- NULL
  mode <- NULL
  output_dir <- NULL
  list_by_wave <- NULL
  overwrite <- FALSE
  allow_missing <- FALSE
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--config")) {
      if (i == length(args)) {
        stop("--config flag requires a path argument")
      }
      config <- args[[i + 1L]]
      i <- i + 2L
    } else if (identical(arg, "--mode")) {
      if (i == length(args)) {
        stop("--mode flag requires a value ('real' or 'proxy')")
      }
      mode <- tolower(args[[i + 1L]])
      if (!mode %in% c("real", "proxy")) {
        stop("Invalid --mode value. Use 'real' or 'proxy'.")
      }
      i <- i + 2L
    } else if (identical(arg, "--output-dir")) {
      if (i == length(args)) {
        stop("--output-dir flag requires a path argument")
      }
      output_dir <- args[[i + 1L]]
      i <- i + 2L
    } else if (identical(arg, "--list-by-wave")) {
      if (i == length(args)) {
        stop("--list-by-wave flag requires a path argument")
      }
      list_by_wave <- args[[i + 1L]]
      i <- i + 2L
    } else if (identical(arg, "--overwrite")) {
      overwrite <- TRUE
      i <- i + 1L
    } else if (identical(arg, "--allow-missing")) {
      allow_missing <- TRUE
      i <- i + 1L
    } else if (identical(arg, "--help") || identical(arg, "-h")) {
      cat(
        "Usage: export_raw_csvs_from_list_by_wave.R [options]\n",
        "\n",
        "Exports participants.csv and outcomes.csv from Wave 1 in list_by_wave.RData.\n",
        "\n",
        "Options:\n",
        "  --config <path>        Path to config YAML (default: reproduced/config/thesis.yml)\n",
        "  --mode <real|proxy>    Override data mode (default: config/SAND_DATA_MODE)\n",
        "  --list-by-wave <path>  Explicit list_by_wave.RData path override\n",
        "  --output-dir <path>    Destination directory (default: active data dir)\n",
        "  --overwrite            Allow overwriting existing CSV outputs\n",
        "  --allow-missing         Fill missing columns with NA instead of error\n",
        "  --help                 Show this message\n",
        sep = ""
      )
      quit(save = "no", status = 0L)
    } else {
      stop(sprintf("Unknown argument: %s", arg))
    }
  }
  list(
    config = config,
    mode = mode,
    output_dir = output_dir,
    list_by_wave = list_by_wave,
    overwrite = overwrite,
    allow_missing = allow_missing
  )
}

load_list_by_wave <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("list_by_wave.RData not found at %s", path))
  }
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  if (!exists("list_by_wave", envir = env)) {
    available <- ls(envir = env)
    detail <- if (length(available)) paste(available, collapse = ", ") else "(none)"
    stop(sprintf("Expected object 'list_by_wave' in %s. Found: %s", path, detail))
  }
  list_by_wave <- get("list_by_wave", envir = env)
  if (!is.list(list_by_wave) || length(list_by_wave) < 1L) {
    stop("list_by_wave must be a non-empty list.")
  }
  list_by_wave
}

dedupe_by_id <- function(df, id_col) {
  if (!id_col %in% names(df)) {
    stop(sprintf("Required identifier column '%s' missing from Wave 1 data.", id_col))
  }
  if (!anyDuplicated(df[[id_col]])) {
    return(df)
  }
  warning(sprintf("Duplicate %s detected in Wave 1; keeping first row per ID.", id_col))
  df[!duplicated(df[[id_col]]), , drop = FALSE]
}

select_columns <- function(df, columns, label, allow_missing = FALSE) {
  missing <- setdiff(columns, names(df))
  if (length(missing) > 0L) {
    msg <- sprintf("Missing %s columns in Wave 1: %s", label, paste(missing, collapse = ", "))
    if (!allow_missing) {
      stop(msg)
    }
    warning(msg)
    for (col in missing) {
      df[[col]] <- NA
    }
  }
  df[, columns, drop = FALSE]
}

write_csv_safe <- function(df, path, overwrite = FALSE) {
  if (file.exists(path) && !overwrite) {
    stop(sprintf("Refusing to overwrite existing file: %s (use --overwrite).", path))
  }
  utils::write.csv(df, path, row.names = FALSE)
}

main <- function() {
  args <- parse_arguments(commandArgs(trailingOnly = TRUE))

  script_path <- get_script_path()
  script_dir <- dirname(script_path)
  repro_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)
  source(file.path(repro_root, "scripts", "utils", "config_loader.R"))

  config_path <- args$config %||% "reproduced/config/thesis.yml"
  cfg <- load_configuration(config_path)

  data_dir <- resolve_data_dir(cfg, mode_override = args$mode, must_exist = FALSE)
  repo_root <- cfg$repo_root

  list_by_wave_path <- resolve_cli_path(args$list_by_wave, repo_root)
  if (is.null(list_by_wave_path)) {
    list_by_wave_path <- file.path(data_dir, "list_by_wave.RData")
  }

  output_dir <- resolve_cli_path(args$output_dir, repo_root)
  if (is.null(output_dir)) {
    output_dir <- data_dir
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  list_by_wave <- load_list_by_wave(list_by_wave_path)
  base_wave <- list_by_wave[[1]]
  if (!is.data.frame(base_wave)) {
    stop("Wave 1 entry in list_by_wave must be a data frame.")
  }

  base_wave <- dedupe_by_id(base_wave, "redcap_survey_identifier")

  participants_cols <- c("redcap_survey_identifier", "age", "sex", "ethnicity", "friend_number")
  outcomes_cols <- c("redcap_survey_identifier", "audit_score", "q1", "q2", "q3", "byaacq_6")

  participants <- select_columns(base_wave, participants_cols, "participants", args$allow_missing)
  outcomes <- select_columns(base_wave, outcomes_cols, "outcomes", args$allow_missing)

  participants_path <- file.path(output_dir, "participants.csv")
  outcomes_path <- file.path(output_dir, "outcomes.csv")

  write_csv_safe(participants, participants_path, args$overwrite)
  write_csv_safe(outcomes, outcomes_path, args$overwrite)

  message("Wrote ", nrow(participants), " participants to ", participants_path)
  message("Wrote ", nrow(outcomes), " outcomes to ", outcomes_path)
}

`%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error exporting CSVs: ", e$message)
      quit(status = 1)
    }
  )
}
