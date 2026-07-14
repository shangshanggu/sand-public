#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

# This bridge script reshapes Chapter 4 prepared datasets into the Chapter 5 NAM input schema
# and writes them into the Chapter 5 outputs directory. It runs as part of the Chapter 4 target
# so that Chapter 5 can operate on real (non-synthetic) data without manual staging.

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
  stop("Unable to determine script path for Chapter 5 bridge export.")
}

script_dir <- dirname(get_script_path())
repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

# Reuse the Chapter 5 preparation helpers to avoid duplication.
source(file.path(repo_root, "analyses", "chapter5_descriptive_norms", "scripts", "01_prepare_chapter5_data.R"))

main <- function() {
  bundle <- load_configuration()
  ensure_chapter_enabled(bundle, "chapter4_data_collection")

  # If Chapter 5 is disabled, skip silently to avoid failing Chapter 4.
  chapter5_enabled <- get_config_value(bundle, "chapters", "chapter5_descriptive_norms", "enabled", default = FALSE)
  if (!isTRUE(chapter5_enabled)) {
    message("[chapter4->chapter5] Chapter 5 disabled in config; skipping bridge export.")
    return(invisible(TRUE))
  }

  chapter5_outputs <- resolve_chapter5_outputs(bundle)
  chapter4_manifest <- resolve_chapter4_manifest(bundle)

  nam_inputs <- prepare_nam_dataset(bundle, chapter4_manifest, chapter5_outputs)
  nam_dataset <- nam_inputs$dataset
  if (nrow(nam_dataset) == 0) {
    stop("Unable to assemble NAM analysis dataset from Chapter 4 sources.")
  }

  metadata <- build_metadata(nam_dataset, bundle, nam_inputs$model_files, nam_inputs$source_tables, chapter4_manifest, nam_inputs$provenance)
  outputs <- write_outputs(nam_dataset, chapter5_outputs, metadata)

  message("[chapter4->chapter5] NAM bridge inputs written to ", outputs$csv)
  message("[chapter4->chapter5] Metadata stored at ", outputs$metadata)
  invisible(outputs)
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error exporting Chapter 5 bridge inputs: ", e$message)
      quit(status = 1)
    }
  )
}
