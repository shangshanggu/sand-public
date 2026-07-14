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
  stop("Unable to determine script path for Chapter 5 data preparation.")
}

script_dir <- dirname(get_script_path())
repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

resolve_chapter5_outputs <- function(bundle) {
  resolve_chapter_output_paths(bundle, "chapter5_descriptive_norms")
}

resolve_chapter4_manifest <- function(bundle) {
  chapter4_paths <- resolve_chapter_output_paths(bundle, "chapter4_data_collection")
  manifest_path <- file.path(chapter4_paths$manifests, "prepared_data_manifest.json")
  if (!file.exists(manifest_path)) {
    stop(sprintf("Missing Chapter 4 manifest at %s. Run Chapter 4 before preparing Chapter 5.", manifest_path))
  }
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  entries <- manifest$entries
  if (is.null(entries) || length(entries) == 0) {
    stop(sprintf("Chapter 4 manifest at %s contained no entries.", manifest_path))
  }
  list(path = manifest_path, entries = entries)
}

map_time_to_wave_label <- function(model_time) {
  lower <- trimws(tolower(model_time))
  if (lower %in% c("time 1", "time1")) return("time1")
  if (lower %in% c("time 2", "time2")) return("time2")
  if (lower %in% c("time 3", "time3")) return("time3")
  stop(sprintf("Unrecognised model_time value: %s", model_time))
}

wave_to_model_time <- function(wave_label) {
  lower <- trimws(tolower(wave_label))
  if (lower == "time1") return("Time 1")
  if (lower == "time2") return("Time 2")
  if (lower == "time3") return("Time 3")
  sprintf("Time %s", gsub("^time", "", wave_label, ignore.case = TRUE))
}

resolve_target_models <- function(chapter4_manifest) {
  entries <- chapter4_manifest$entries
  candidates <- Filter(function(entry) {
    entry$imputation_method == "LOCF" &&
      entry$typical_definition == "mean" &&
      !identical(entry$wave, "baseline")
  }, entries)

  if (!length(candidates)) {
    stop("Chapter 4 manifest does not contain LOCF + mean prepared datasets for time waves.")
  }

  wave_order <- c("time1", "time2", "time3")
  candidates <- candidates[order(match(vapply(candidates, function(e) e$wave, character(1)), wave_order))]

  lapply(seq_along(candidates), function(i) {
    entry <- candidates[[i]]
    wave_label <- entry$wave
    list(
      model_time = wave_to_model_time(wave_label),
      model_index = 100L + i,
      entry = entry
    )
  })
}

build_model_dataset <- function(bundle, target, output_dir) {
  model_index <- target$model_index
  model_time <- target$model_time
  entry <- target$entry
  data_path <- resolve_repo_path(bundle, entry$path, must_exist = TRUE)
  dataset <- read.csv(data_path, stringsAsFactors = FALSE)

  required_cols <- c("redcap_survey_identifier", "misperception_audit_c_global", "misperception_audit_score_peer", "audit_score")
  missing_cols <- setdiff(required_cols, colnames(dataset))
  if (length(missing_cols) > 0) {
    stop(sprintf("Dataset %s missing required columns: %s", data_path, paste(missing_cols, collapse = ", ")))
  }

  model_dataset <- data.frame(
    participant_id = dataset$redcap_survey_identifier,
    misperception_audit_c_global = as.numeric(dataset$misperception_audit_c_global),
    misperception_audit_c_peer = as.numeric(dataset$misperception_audit_score_peer),
    audit_score = as.numeric(dataset$audit_score),
    stringsAsFactors = FALSE
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path(output_dir, sprintf("model_data_%s.csv", model_index))
  write.csv(model_dataset, output_path, row.names = FALSE)

  list(
    data = transform(model_dataset, time_period = model_time, model_index = model_index),
    output_path = output_path,
    source_table = relative_repo_path(data_path, bundle$repo_root)
  )
}

validate_existing_model_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  required_cols <- c("participant_id", "misperception_audit_c_global", "misperception_audit_c_peer", "audit_score")
  dataset <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(...) NULL)
  if (is.null(dataset)) return(FALSE)
  all(required_cols %in% colnames(dataset))
}

load_existing_inputs <- function(bundle, paths) {
  input_path <- file.path(paths$tables, "nam_analysis_input.csv")
  model_dir <- file.path(paths$data, "model_data")
  if (!file.exists(input_path) || !dir.exists(model_dir)) {
    return(NULL)
  }

  dataset <- tryCatch(read.csv(input_path, stringsAsFactors = FALSE), error = function(...) NULL)
  required_cols <- c("participant_id", "misperception_audit_c_global", "misperception_audit_c_peer", "audit_score", "time_period", "model_index")
  if (is.null(dataset) || !all(required_cols %in% colnames(dataset))) {
    return(NULL)
  }

  model_files <- list.files(model_dir, pattern = "^model_data_[0-9]+\\.csv$", full.names = TRUE)
  if (!length(model_files)) {
    return(NULL)
  }

  valid_models <- vapply(model_files, validate_existing_model_file, logical(1))
  if (!all(valid_models)) {
    return(NULL)
  }

  list(
    dataset = dataset,
    model_files = model_files,
    source_tables = model_files,
    provenance = "preexisting"
  )
}

prepare_nam_dataset <- function(bundle, chapter4_manifest, paths) {
  targets <- resolve_target_models(chapter4_manifest)
  if (!length(targets)) {
    stop("No NAM targets available from Chapter 4 manifest.")
  }
  model_output_dir <- file.path(paths$data, "model_data")

  rows <- lapply(targets, function(target) {
    build_model_dataset(bundle, target, model_output_dir)
  })

  combined <- do.call(rbind, lapply(rows, function(x) x$data))
  combined$time_period <- factor(combined$time_period, levels = vapply(targets, function(t) t$model_time, character(1)))
  rownames(combined) <- NULL
  list(
    dataset = combined,
    model_files = vapply(rows, function(x) x$output_path, character(1)),
    source_tables = unique(vapply(rows, function(x) x$source_table, character(1))),
    provenance = "chapter4_manifest"
  )
}

write_outputs <- function(dataset, paths, metadata) {
  csv_path <- file.path(paths$tables, "nam_analysis_input.csv")
  rds_path <- file.path(paths$data, "nam_analysis_input.rds")
  json_path <- file.path(paths$manifests, "nam_analysis_metadata.json")

  write.csv(dataset, csv_path, row.names = FALSE)
  saveRDS(dataset, rds_path)
  write_json(metadata, json_path, auto_unbox = TRUE, pretty = TRUE)

  list(csv = csv_path, rds = rds_path, metadata = json_path)
}

build_metadata <- function(dataset, bundle, model_files, source_tables, chapter4_manifest, provenance) {
  model_paths <- vapply(model_files, function(path) relative_repo_path(path, bundle$repo_root), character(1))
  source_paths <- vapply(source_tables, function(path) relative_repo_path(path, bundle$repo_root), character(1))

  chapter4_manifest_path <- if (!is.null(chapter4_manifest)) relative_repo_path(chapter4_manifest$path, bundle$repo_root) else NULL

  list(
    generated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    sources = list(
      chapter4_manifest = chapter4_manifest_path,
      model_datasets = model_paths,
      chapter4_tables = source_paths
    ),
    rows = nrow(dataset),
    columns = ncol(dataset),
    provenance = provenance,
    notes = if (provenance == "chapter4_manifest") {
      "NAM inputs regenerated from Chapter 4 prepared datasets (LOCF, mean); model indices assigned sequentially."
    } else {
      "NAM inputs reused from preexisting Chapter 5 inputs matching the required schema."
    }
  )
}

main <- function() {
  bundle <- load_configuration()
  ensure_chapter_enabled(bundle, "chapter5_descriptive_norms")

  chapter5_outputs <- resolve_chapter5_outputs(bundle)
  chapter4_manifest <- resolve_chapter4_manifest(bundle)

  existing <- load_existing_inputs(bundle, chapter5_outputs)
  nam_inputs <- if (!is.null(existing)) {
    message("[chapter5] Reusing existing NAM inputs at ", file.path(chapter5_outputs$tables, "nam_analysis_input.csv"))
    existing
  } else {
    prepare_nam_dataset(bundle, chapter4_manifest, chapter5_outputs)
  }
  nam_dataset <- nam_inputs$dataset
  if (nrow(nam_dataset) == 0) {
    stop("Unable to assemble NAM analysis dataset from Chapter 4 sources.")
  }

  metadata <- build_metadata(nam_dataset, bundle, nam_inputs$model_files, nam_inputs$source_tables, chapter4_manifest, nam_inputs$provenance)
  outputs <- write_outputs(nam_dataset, chapter5_outputs, metadata)

  message("[chapter5] NAM analysis inputs written to ", outputs$csv)
  message("[chapter5] Metadata stored at ", outputs$metadata)
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error preparing Chapter 5 data: ", e$message)
      quit(status = 1)
    }
  )
}
