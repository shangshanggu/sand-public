#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

`%||%` <- function(x, y) {
  if (!is.null(x) && length(x) > 0) {
    x
  } else {
    y
  }
}

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
  stop("Unable to determine script path for Chapter 6 data preparation.")
}

script_dir <- dirname(get_script_path())
repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

normalise_wave_label <- function(wave) {
  lower <- tolower(wave)
  lower <- gsub("\\s+", "", lower)
  if (lower == "baseline") {
    return("Baseline")
  }
  if (grepl("^time[0-9]+$", lower)) {
    index <- sub("^time", "", lower)
    return(paste("Time", index))
  }
  wave
}

normalise_scenarios <- function(raw_scenarios) {
  if (!is.list(raw_scenarios) || length(raw_scenarios) == 0) {
    stop("Chapter 6 configuration does not define any scenarios.")
  }
  scenario_names <- names(raw_scenarios)
  items <- vector("list", length(raw_scenarios))
  for (i in seq_along(raw_scenarios)) {
    scenario <- raw_scenarios[[i]]
    if (!is.list(scenario)) {
      ref <- i
      if (!is.null(scenario_names) && length(scenario_names) >= i) {
        candidate <- scenario_names[[i]]
        if (!is.null(candidate) && nzchar(candidate)) {
          ref <- candidate
        }
      }
      stop(sprintf("Scenario entry %s is not a mapping.", ref))
    }
    key_name <- if (is.null(scenario_names)) NULL else scenario_names[[i]]
    if ((is.null(scenario$key) || !nzchar(as.character(scenario$key))) && !is.null(key_name) && nzchar(key_name)) {
      scenario$key <- key_name
    }
    if (is.null(scenario$outcome_transform) || !nzchar(as.character(scenario$outcome_transform))) {
      scenario$outcome_transform <- "identity"
    }
    items[[i]] <- scenario
  }
  items
}

load_manifest <- function(path) {
  manifest <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  entries <- manifest$entries
  if (length(entries) == 0) {
    stop("Chapter 4 manifest contains no entries for approval trajectories.")
  }
  entries
}

find_manifest_entry <- function(entries, imputation, typical, wave) {
  matches <- Filter(function(entry) {
    identical(tolower(entry$imputation_method), tolower(imputation)) &&
      identical(tolower(entry$typical_definition), tolower(typical)) &&
      identical(tolower(entry$wave), tolower(wave))
  }, entries)
  if (length(matches) == 0) {
    stop(sprintf(
      "No manifest entry found for imputation=%s, typical=%s, wave=%s",
      imputation,
      typical,
      wave
    ))
  }
  matches[[1]]
}

load_manifest_dataset <- function(bundle, entry) {
  entry_path <- resolve_repo_path(bundle, entry$path, must_exist = TRUE)
  read.csv(entry_path, stringsAsFactors = FALSE)
}

ensure_columns <- function(dataset, required, context) {
  missing <- setdiff(required, names(dataset))
  if (length(missing) > 0) {
    stop(sprintf(
      "Dataset %s is missing required columns: %s",
      context,
      paste(missing, collapse = ", ")
    ))
  }
}

transform_outcome <- function(values, transform_name) {
  numeric_values <- as.numeric(values)
  transform_key <- tolower(as.character(transform_name %||% "identity"))
  if (transform_key %in% c("identity", "none")) {
    return(numeric_values)
  }
  if (transform_key %in% c("positive_to_one", "positive-to-one", "binarize_positive")) {
    return(ifelse(is.na(numeric_values), 0, ifelse(numeric_values > 0, 1, 0)))
  }
  warning(sprintf("Unknown outcome transform '%s'; defaulting to identity.", transform_name))
  numeric_values
}

extract_scenario_rows <- function(dataset, scenario, time_label) {
  ensure_columns(dataset, c("redcap_survey_identifier", scenario$approval_column, scenario$outcome_column), scenario$key)

  global_col <- scenario$misperception_global_column %||% NA_character_
  peer_col <- scenario$misperception_peer_column %||% NA_character_

  global_values <- if (!is.na(global_col) && global_col %in% names(dataset)) {
    as.numeric(dataset[[global_col]])
  } else {
    rep(NA_real_, nrow(dataset))
  }

  peer_values <- if (!is.na(peer_col) && peer_col %in% names(dataset)) {
    as.numeric(dataset[[peer_col]])
  } else {
    rep(NA_real_, nrow(dataset))
  }

  data.frame(
    participant_id = dataset$redcap_survey_identifier,
    time_period = time_label,
    scenario_key = scenario$key,
    scenario_label = scenario$label,
    imputation_method = scenario$imputation_method,
    typical_definition = scenario$typical_definition,
    approval_value = as.numeric(dataset[[scenario$approval_column]]),
    misperception_global = global_values,
    misperception_peer = peer_values,
    outcome_value = transform_outcome(dataset[[scenario$outcome_column]], scenario$outcome_transform),
    stringsAsFactors = FALSE
  )
}

collect_longitudinal_rows <- function(bundle, entries, scenarios) {
  rows <- list()
  idx <- 1
  for (scenario in scenarios) {
    waves <- scenario$waves
    if (is.null(waves) || length(waves) == 0) {
      waves <- c("baseline", "time1", "time2", "time3")
    }
    for (wave in waves) {
      entry <- find_manifest_entry(entries, scenario$imputation_method, scenario$typical_definition, wave)
      dataset <- load_manifest_dataset(bundle, entry)
      time_label <- normalise_wave_label(entry$wave %||% wave)
      scenario_rows <- extract_scenario_rows(dataset, scenario, time_label)
      scenario_rows$manifest_wave <- entry$wave
      scenario_rows$source_rows <- nrow(dataset)
      rows[[idx]] <- scenario_rows
      idx <- idx + 1
    }
  }
  if (length(rows) == 0) {
    stop("No approval data collected for Chapter 6 scenarios.")
  }
  combined <- do.call(rbind, rows)
  rownames(combined) <- NULL
  combined
}

write_outputs <- function(data, rds_path, csv_path, metadata_path, bundle, manifest_path, scenarios) {
  dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data, rds_path)
  write.csv(data, csv_path, row.names = FALSE)

  scenario_metadata <- lapply(scenarios, function(scenario) {
    list(
      key = scenario$key,
      label = scenario$label,
      imputation_method = scenario$imputation_method,
      typical_definition = scenario$typical_definition,
      waves = scenario$waves %||% list("baseline", "time1", "time2", "time3"),
      columns = list(
        approval = scenario$approval_column,
        outcome = scenario$outcome_column,
        misperception_global = scenario$misperception_global_column %||% NA,
        misperception_peer = scenario$misperception_peer_column %||% NA
      ),
      outcome_transform = scenario$outcome_transform
    )
  })

  metadata <- list(
    generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    manifest = relative_repo_path(manifest_path, bundle$repo_root),
    rows = nrow(data),
    participants = length(unique(data$participant_id)),
    scenarios = scenario_metadata,
    time_periods = unique(as.character(data$time_period))
  )

  write_json(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)

  list(rds = rds_path, csv = csv_path, metadata = metadata_path)
}

main <- function() {
  bundle <- load_configuration()
  ensure_chapter_enabled(bundle, "chapter6_injunctive_norms")

  chapter4_paths <- resolve_chapter_output_paths(bundle, "chapter4_data_collection")
  manifest_path <- file.path(chapter4_paths$manifests, "prepared_data_manifest.json")
  if (!file.exists(manifest_path)) {
    stop(sprintf("Missing prepared_data_manifest.json at %s. Run Chapter 4 exports before Chapter 6.", manifest_path))
  }

  raw_scenarios <- get_config_value(bundle, "chapters", "chapter6_injunctive_norms", "scenarios", required = TRUE)
  scenarios <- normalise_scenarios(raw_scenarios)

  chapter6_paths <- resolve_chapter_output_paths(bundle, "chapter6_injunctive_norms")

  longitudinal_rel <- get_config_value(bundle, "chapters", "chapter6_injunctive_norms", "longitudinal_data", required = TRUE)
  longitudinal_rds <- resolve_repo_path(bundle, longitudinal_rel, must_exist = FALSE)
  longitudinal_csv <- if (grepl("\\.rds$", longitudinal_rds, ignore.case = TRUE)) {
    sub("\\.rds$", ".csv", longitudinal_rds, ignore.case = TRUE)
  } else {
    paste0(longitudinal_rds, ".csv")
  }
  metadata_path <- file.path(chapter6_paths$manifests, "approval_longitudinal_metadata.json")

  manifest_entries <- load_manifest(manifest_path)
  longitudinal_data <- collect_longitudinal_rows(bundle, manifest_entries, scenarios)

  outputs <- write_outputs(longitudinal_data, longitudinal_rds, longitudinal_csv, metadata_path, bundle, manifest_path, scenarios)

  message("[chapter6] Approval longitudinal dataset written to ", outputs$csv)
  message("[chapter6] Metadata stored at ", outputs$metadata)
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error preparing Chapter 6 approval longitudinal data: ", e$message)
      quit(status = 1)
    }
  )
}
