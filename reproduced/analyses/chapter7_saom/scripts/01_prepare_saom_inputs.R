#!/usr/bin/env Rscript

suppressWarnings(suppressMessages({
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The 'yaml' package is required. Install it via install.packages('yaml').", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required. Install it via install.packages('jsonlite').", call. = FALSE)
  }
}))

library(jsonlite)
library(yaml)

digest_available <- requireNamespace("digest", quietly = TRUE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (is.character(x) && length(x) == 1 && !nzchar(x)) {
    return(y)
  }
  x
}

log_message <- function(...) {
  message(sprintf(...))
}

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) == 0) {
    return(normalizePath("."))
  }
  normalizePath(dirname(script_path))
}

repo_root <- normalizePath(file.path(get_script_dir(), "..", "..", ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

config_path <- file.path("config", "thesis.yml")
if (!file.exists(config_path)) {
  stop(sprintf("Configuration file not found at %s", config_path))
}

source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))
config_bundle <- load_configuration(config_path)
config <- config_bundle$config
chapter_cfg <- config$chapters$chapter7_saom
if (is.null(chapter_cfg)) {
  stop("chapter7_saom configuration block is missing from reproduced/config/thesis.yml")
}

output_paths <- resolve_chapter_output_paths(config_bundle, "chapter7_saom")
outputs_dir <- output_paths$base

cache_rel <- chapter_cfg$cache_dir %||% file.path(chapter_cfg$outputs_dir, "cache")
cache_dir <- resolve_repo_path(config_bundle, cache_rel)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

logs_rel <- config$rsiena$diagnostics$diagnostics_dir %||% "outputs/chapter7/logs"
logs_dir <- resolve_repo_path(config_bundle, logs_rel)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

expected_inputs <- unique(c(
  chapter_cfg$required_inputs %||% character(),
  chapter_cfg$baseline_model$path %||% character(),
  chapter_cfg$precomputed_results %||% character()
))
expected_inputs <- expected_inputs[!is.na(expected_inputs) & nzchar(expected_inputs)]

normalize_path <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return(NA_character_)
  }
  if (grepl("^/", path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  tryCatch(
    resolve_repo_path(config_bundle, path, must_exist = FALSE),
    error = function(...) normalizePath(file.path(repo_root, path), winslash = "/", mustWork = FALSE)
  )
}

collect_file_metadata <- function(path_relative) {
  absolute <- normalize_path(path_relative)
  exists_flag <- !is.na(absolute) && file.exists(absolute)
  list(
    path = path_relative,
    absolute_path = if (is.na(absolute)) NULL else absolute,
    exists = exists_flag,
    size_bytes = if (exists_flag) unclass(file.info(absolute)$size) else NA_integer_,
    checksum_sha256 = if (exists_flag && digest_available) digest::digest(file = absolute, algo = "sha256") else NA_character_
  )
}

manifest <- list(
  generated_at = format(Sys.time(), tz = config$project$timezone %||% "UTC"),
  inputs = lapply(expected_inputs, collect_file_metadata)
)

manifest_path <- file.path(output_paths$manifests, "saom_data_manifest.json")
jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE, na = "null")

missing_inputs <- vapply(manifest$inputs, function(entry) !isTRUE(entry$exists), logical(1))
if (any(missing_inputs)) {
  missing_paths <- vapply(manifest$inputs[missing_inputs], function(entry) entry$path %||% "<unknown>", character(1))
  log_message("SAOM data preparation completed with missing inputs: %s", paste(missing_paths, collapse = ", "))
} else {
  log_message("SAOM data preparation manifest generated successfully at %s", manifest_path)
}

status <- list(
  generated_at = manifest$generated_at,
  manifest_path = manifest_path,
  missing_inputs = manifest$inputs[missing_inputs]
)

if (!digest_available) {
  status$missing_packages <- list("digest")
  status$notes <- c(status$notes %||% character(), "digest package not installed; file checksums omitted")
  log_message("SAOM data preparation skipped checksum generation because 'digest' is not installed.")
}
jsonlite::write_json(status, file.path(logs_dir, "saom_data_preparation_status.json"), pretty = TRUE, auto_unbox = TRUE, na = "null")
