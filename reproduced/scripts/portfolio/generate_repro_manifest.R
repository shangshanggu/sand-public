#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# generate_repro_manifest.R
#
# Build a reproducibility manifest for Chapters 4-7 containing:
# - input checksums
# - output checksums
# - RNG seed metadata
# - execution timestamps
#
# Output:
#   reproduced/outputs/portfolio/reproducibility_manifest.json
# -----------------------------------------------------------------------------

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_idx <- grep(file_arg, args)
  if (length(file_idx) > 0) {
    for (idx in file_idx) {
      candidate <- sub(file_arg, "", args[idx])
      if (basename(candidate) == "generate_repro_manifest.R" && file.exists(candidate)) {
        return(dirname(normalizePath(candidate, winslash = "/", mustWork = TRUE)))
      }
    }
  }
  for (i in seq_len(sys.nframe())) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(...) NULL)
    if (!is.null(ofile) && basename(ofile) == "generate_repro_manifest.R") {
      return(dirname(normalizePath(ofile, winslash = "/", mustWork = TRUE)))
    }
  }
  getwd()
}

script_dir <- get_script_dir()
common_candidates <- c(
  file.path(script_dir, "..", "..", "R", "common.R"),
  "R/common.R",
  "reproduced/R/common.R"
)
loader_candidates <- c(
  file.path(script_dir, "..", "utils", "config_loader.R"),
  "scripts/utils/config_loader.R",
  "reproduced/scripts/utils/config_loader.R"
)

for (path in unique(common_candidates)) {
  if (file.exists(path)) {
    source(path, local = TRUE)
    break
  }
}
for (path in unique(loader_candidates)) {
  if (file.exists(path)) {
    source(path, local = TRUE)
    break
  }
}

if (!exists("format_timestamp", mode = "function")) {
  format_timestamp <- function(time = Sys.time(), tz = "UTC") {
    posix <- as.POSIXct(time, tz = tz)
    paste0(format(posix, "%Y-%m-%dT%H:%M:%SZ", tz = tz))
  }
}

if (!exists("relative_repo_path", mode = "function")) {
  relative_repo_path <- function(path, repo_root) {
    repo_norm <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
    path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
    prefix <- paste0(repo_norm, "/")
    if (startsWith(path_norm, prefix)) {
      return(substring(path_norm, nchar(prefix) + 1L))
    }
    path_norm
  }
}

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

resolve_config_path <- function() {
  candidates <- c("config/thesis.yml", "reproduced/config/thesis.yml")
  for (candidate in candidates) {
    if (file.exists(candidate)) return(candidate)
  }
  stop("thesis.yml not found. Cannot generate reproducibility manifest.")
}

resolve_data_mode <- function(bundle) {
  mode <- get_config_value(bundle, "data", "mode", default = "real")
  env_mode <- Sys.getenv("SAND_DATA_MODE", "")
  if (nzchar(env_mode)) mode <- env_mode
  tolower(as.character(mode))
}

list_recursive_files <- function(dir_path) {
  if (!dir.exists(dir_path)) return(character(0))
  files <- list.files(
    dir_path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  if (length(files) == 0) return(character(0))
  info <- file.info(files)
  files[!info$isdir]
}

compute_sha256 <- function(path) {
  if (!file.exists(path)) return(NULL)

  if (requireNamespace("digest", quietly = TRUE)) {
    return(unname(digest::digest(file = path, algo = "sha256", serialize = FALSE)))
  }

  sha256sum_bin <- Sys.which("sha256sum")
  if (nzchar(sha256sum_bin)) {
    out <- tryCatch(system2(sha256sum_bin, args = path, stdout = TRUE, stderr = FALSE),
                    error = function(...) character(0))
    if (length(out) > 0) {
      return(strsplit(out[[1]], "\\s+")[[1]][1])
    }
  }

  shasum_bin <- Sys.which("shasum")
  if (nzchar(shasum_bin)) {
    out <- tryCatch(system2(shasum_bin, args = c("-a", "256", path),
                            stdout = TRUE, stderr = FALSE),
                    error = function(...) character(0))
    if (length(out) > 0) {
      return(strsplit(out[[1]], "\\s+")[[1]][1])
    }
  }

  stop(
    sprintf(
      "Unable to compute SHA-256 for %s. Install R package 'digest' or provide sha256sum/shasum.",
      path
    )
  )
}

collect_checksums <- function(paths, repo_root) {
  checksums <- list()
  missing <- character(0)

  if (length(paths) == 0) {
    return(list(checksums = checksums, missing = missing))
  }

  for (path in unique(paths)) {
    rel <- relative_repo_path(path, repo_root)
    if (!file.exists(path)) {
      missing <- c(missing, rel)
      next
    }
    checksums[[rel]] <- compute_sha256(path)
  }

  list(
    checksums = checksums,
    missing = sort(unique(missing))
  )
}

resolve_chapter_seed <- function(bundle, chapter_key) {
  seed <- switch(
    chapter_key,
    chapter5_descriptive_norms = get_config_value(bundle, "chapters", chapter_key, "nam", "seed", default = NULL),
    chapter7_saom = get_config_value(bundle, "rsiena", "estimation", "seed", default = NULL) %||%
      get_config_value(bundle, "rsiena", "project_seed", default = NULL),
    get_config_value(bundle, "chapters", chapter_key, "seed", default = NULL)
  )
  if (is.null(seed) || !is.numeric(seed)) return(NULL)
  as.integer(seed)
}

resolve_execution_timestamp <- function(output_dir) {
  log_files <- list_recursive_files(file.path(output_dir, "logs"))
  if (length(log_files) > 0) {
    mtime <- file.info(log_files)$mtime
    mtime <- mtime[!is.na(mtime)]
    if (length(mtime) > 0) {
      return(format_timestamp(max(mtime)))
    }
  }

  chapter_files <- list_recursive_files(output_dir)
  if (length(chapter_files) > 0) {
    mtime <- file.info(chapter_files)$mtime
    mtime <- mtime[!is.na(mtime)]
    if (length(mtime) > 0) {
      return(format_timestamp(max(mtime)))
    }
  }

  NULL
}

build_chapter_manifest <- function(bundle, chapter_key) {
  repo_root <- bundle$repo_root
  output_rel <- get_config_value(bundle, "chapters", chapter_key, "outputs_dir", default = NULL)

  if (is.null(output_rel) || !nzchar(output_rel)) {
    return(list(
      chapter = chapter_key,
      status = "config_missing",
      input_checksums = list(),
      output_checksums = list(),
      missing_inputs = character(0),
      rng_seed = NULL,
      execution_timestamp = NULL
    ))
  }

  output_dir <- resolve_repo_path(bundle, output_rel, must_exist = FALSE)
  required_inputs <- get_config_value(bundle, "chapters", chapter_key, "required_inputs", default = character(0))
  if (is.null(required_inputs)) required_inputs <- character(0)
  required_inputs <- as.character(required_inputs)
  input_paths <- vapply(required_inputs, function(x) resolve_repo_path(bundle, x, must_exist = FALSE), character(1))

  output_files <- list_recursive_files(output_dir)
  input_result <- collect_checksums(input_paths, repo_root)
  output_result <- collect_checksums(output_files, repo_root)

  status <- if (length(output_files) > 0) "executed" else "not_run"
  seed_value <- resolve_chapter_seed(bundle, chapter_key)
  timestamp_value <- resolve_execution_timestamp(output_dir)
  if (is.null(seed_value)) seed_value <- NA_integer_
  if (is.null(timestamp_value) || !nzchar(timestamp_value)) timestamp_value <- NA_character_

  list(
    chapter = chapter_key,
    status = status,
    input_checksums = input_result$checksums,
    output_checksums = output_result$checksums,
    missing_inputs = unname(as.list(input_result$missing)),
    rng_seed = seed_value,
    execution_timestamp = timestamp_value
  )
}

build_manifest <- function(bundle) {
  chapter_keys <- c(
    "chapter4_data_collection",
    "chapter5_descriptive_norms",
    "chapter6_injunctive_norms",
    "chapter7_saom"
  )

  chapters <- lapply(chapter_keys, function(key) build_chapter_manifest(bundle, key))

  list(
    generated_at = format_timestamp(),
    pipeline_version = get_config_value(bundle, "project", "version", default = "unknown"),
    data_mode = resolve_data_mode(bundle),
    chapters = chapters
  )
}

write_manifest <- function(bundle, manifest) {
  output_dir <- file.path(bundle$repo_root, "reproduced", "outputs", "portfolio")
  output_path <- file.path(output_dir, "reproducibility_manifest.json")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    manifest,
    output_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
  output_path
}

main <- function() {
  config_path <- resolve_config_path()
  bundle <- load_configuration(config_path)
  manifest <- build_manifest(bundle)
  out_path <- write_manifest(bundle, manifest)

  message("[portfolio-manifest] Wrote reproducibility manifest to ", out_path)
  invisible(manifest)
}

if (identical(environment(), globalenv()) && !length(sys.frames())) {
  tryCatch(main(), error = function(e) {
    message("Error: ", e$message)
    quit(status = 1)
  })
}
