#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

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
  stop("Unable to determine script path for longitudinal export.")
}

script_dir <- dirname(get_script_path())
repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

usage <- function() {
  message(
    "Usage: Rscript reproduced/analyses/chapter4_data_collection/scripts/03_export_norms_longitudinal.R [options]\\n",
    "\\n",
    "Options:\\n",
    "  --config <path>     Path to thesis configuration (default: config/thesis.yml)\\n",
    "  --manifest <path>   Path to prepared_data_manifest.json (defaults from config)\\n",
    "  --output <path>     Where to write norms_longitudinal.rds (defaults from config)\\n",
    "  --summary <path>    Optional CSV path for the longitudinal summary table\\n",
    "  --help              Display this message\\n"
  )
}

parse_args <- function(args) {
  opts <- list(
    config = "config/thesis.yml",
    manifest = NULL,
    output = NULL,
    summary = NULL
  )
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--config") {
      if (i == length(args)) stop("--config requires a path argument.")
      i <- i + 1
      opts$config <- args[[i]]
    } else if (arg == "--manifest") {
      if (i == length(args)) stop("--manifest requires a path argument.")
      i <- i + 1
      opts$manifest <- args[[i]]
    } else if (arg == "--output") {
      if (i == length(args)) stop("--output requires a path argument.")
      i <- i + 1
      opts$output <- args[[i]]
    } else if (arg == "--summary") {
      if (i == length(args)) stop("--summary requires a path argument.")
      i <- i + 1
      opts$summary <- args[[i]]
    } else if (arg %in% c("--help", "-h")) {
      usage()
      quit(status = 0)
    } else {
      stop(sprintf("Unknown argument: %s", arg))
    }
    i <- i + 1
  }
  opts
}

normalise_wave_label <- function(wave) {
  lower <- tolower(wave)
  if (lower == "baseline") {
    return("Baseline")
  }
  if (grepl("^time[0-9]+$", lower)) {
    index <- gsub("^time", "", lower)
    return(paste("Time", index))
  }
  wave
}

load_manifest_entries <- function(manifest_path) {
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  entries <- manifest$entries
  if (length(entries) == 0) {
    stop("Chapter 4 manifest contains no entries to export.")
  }
  entries
}

prepare_entry_dataframe <- function(entry, repo_root) {
  data_path <- normalizePath(file.path(repo_root, entry$path), winslash = "/", mustWork = TRUE)
  dataset <- read.csv(data_path, stringsAsFactors = FALSE)

  required_columns <- c(
    "redcap_survey_identifier",
    "redcap_event_name",
    "audit_score",
    "misperception_audit_c_global",
    "misperception_audit_score_peer"
  )
  missing_cols <- setdiff(required_columns, colnames(dataset))
  if (length(missing_cols) > 0) {
    warning(sprintf("Skipping manifest entry %s; missing columns: %s", data_path, paste(missing_cols, collapse = ", ")))
    return(NULL)
  }

  data.frame(
    participant_id = dataset$redcap_survey_identifier,
    redcap_event_name = dataset$redcap_event_name,
    audit_score = as.numeric(dataset$audit_score),
    global_misperception = as.numeric(dataset$misperception_audit_c_global),
    peer_misperception = as.numeric(dataset$misperception_audit_score_peer),
    imputation_method = entry$imputation_method,
    typical_definition = entry$typical_definition,
    wave = entry$wave,
    stringsAsFactors = FALSE
  )
}

build_longitudinal_dataset <- function(entries, repo_root) {
  rows <- lapply(entries, prepare_entry_dataframe, repo_root = repo_root)
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    stop("No Chapter 4 datasets contained the required misperception columns.")
  }
  combined <- do.call(rbind, rows)
  combined$time_period <- vapply(combined$wave, normalise_wave_label, character(1))
  combined$specification <- paste(combined$imputation_method, combined$typical_definition, sep = "_")
  combined <- combined[order(combined$participant_id, combined$time_period, combined$specification), ]
  rownames(combined) <- NULL
  combined
}

summarise_dataset <- function(dataset) {
  specs <- unique(dataset$specification)
  periods <- unique(dataset$time_period)
  summary_rows <- list()
  idx <- 1
  for (spec in specs) {
    for (period in periods) {
      subset_rows <- dataset[dataset$specification == spec & dataset$time_period == period, , drop = FALSE]
      if (nrow(subset_rows) == 0) {
        next
      }
      summary_rows[[idx]] <- data.frame(
        specification = spec,
        time_period = period,
        participants = length(unique(subset_rows$participant_id)),
        audit_score_mean = mean(subset_rows$audit_score, na.rm = TRUE),
        global_misperception_mean = mean(subset_rows$global_misperception, na.rm = TRUE),
        peer_misperception_mean = mean(subset_rows$peer_misperception, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  if (length(summary_rows) == 0) {
    data.frame()
  } else {
    do.call(rbind, summary_rows)
  }
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- parse_args(args)

  config_path <- resolve_existing_path(opts$config)
  bundle <- load_configuration(config_path)
  opts$config <- bundle$config_path
  ensure_chapter_enabled(bundle, "chapter4_data_collection")

  output_paths <- resolve_chapter_output_paths(bundle, "chapter4_data_collection")
  manifest_path <- if (is.null(opts$manifest)) {
    file.path(output_paths$manifests, "prepared_data_manifest.json")
  } else {
    normalizePath(opts$manifest, winslash = "/", mustWork = TRUE)
  }

  if (!file.exists(manifest_path)) {
    stop(sprintf("Missing prepared_data_manifest.json at %s. Run 01_data_preparation_norms.R before exporting longitudinal norms.", manifest_path))
  }

  entries <- load_manifest_entries(manifest_path)
  dataset <- build_longitudinal_dataset(entries, bundle$repo_root)
  output_path <- if (is.null(opts$output)) {
    file.path(output_paths$data, "norms_longitudinal.rds")
  } else {
    normalizePath(opts$output, winslash = "/", mustWork = FALSE)
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(dataset, output_path)

  summary_tbl <- summarise_dataset(dataset)
  message("[chapter4] norms_longitudinal.rds created at ", output_path)
  if (nrow(summary_tbl) > 0) {
    message("[chapter4] Summary:")
    print(summary_tbl, row.names = FALSE)
    if (!is.null(opts$summary)) {
      summary_path <- normalizePath(opts$summary, winslash = "/", mustWork = FALSE)
      dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(summary_tbl, summary_path, row.names = FALSE)
      message("[chapter4] Summary table written to ", summary_path)
    }
  } else {
    message("[chapter4] Summary: dataset contains no rows after aggregation.")
  }
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error exporting longitudinal norms: ", e$message)
      quit(status = 1)
    }
  )
}
