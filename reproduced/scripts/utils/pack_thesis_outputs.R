#!/usr/bin/env Rscript

format_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

find_repo_root <- function() {
  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    candidate <- file.path(current, "reproduced", "config", "thesis.yml")
    if (file.exists(candidate)) {
      return(normalizePath(current, winslash = "/", mustWork = TRUE))
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    current <- parent
  }
  stop("Unable to locate repository root containing reproduced/config/thesis.yml")
}

source_config_loader <- function(repo_root) {
  loader_path <- file.path(repo_root, "reproduced", "scripts", "utils", "config_loader.R")
  if (!file.exists(loader_path)) {
    stop("config_loader.R is missing; cannot continue.")
  }
  source(loader_path)
}

parse_arguments <- function(args) {
  config <- NULL
  quiet <- FALSE
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--config")) {
      if (i == length(args)) {
        stop("--config flag requires a path argument")
      }
      config <- args[[i + 1L]]
      i <- i + 2L
    } else if (identical(arg, "--quiet")) {
      quiet <- TRUE
      i <- i + 1L
    } else if (identical(arg, "--help") || identical(arg, "-h")) {
      cat(
        "Usage: pack_thesis_outputs.R [--config <path>] [--quiet]\\n",
        "\\n",
        "Aggregates tables and figures exported by chapter pipelines into",
        " central thesis directories and records manifests for downstream",
        " LaTeX/Quarto builds.\\n",
        sep = ""
      )
      quit(save = "no", status = 0L)
    } else {
      stop(sprintf("Unknown argument: %s", arg))
    }
  }
  list(config = config, quiet = quiet)
}

ensure_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) {
    return(invisible(TRUE))
  }
  stop(
    sprintf(
      "Missing required R packages: %s. Install them via `make env-renv`.",
      paste(missing, collapse = ", ")
    )
  )
}

relative_path <- function(path, base) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  base <- normalizePath(base, winslash = "/", mustWork = TRUE)
  sub(paste0("^", base, "/"), "", path)
}

sanitize_component <- function(value) {
  value <- gsub("[/\\]+", "__", value)
  gsub("[^A-Za-z0-9_.-]+", "_", value)
}

glob_to_regex <- function(pattern) {
  utils::glob2rx(pattern, trim.tail = FALSE)
}

collect_assets <- function(asset_dir, patterns) {
  if (!length(patterns) || !dir.exists(asset_dir)) {
    return(character())
  }
  results <- character()
  for (pattern in patterns) {
    regex <- glob_to_regex(pattern)
    matches <- list.files(
      asset_dir,
      pattern = regex,
      recursive = TRUE,
      full.names = TRUE,
      include.dirs = FALSE
    )
    if (length(matches)) {
      results <- c(results, matches)
    }
  }
  unique(normalizePath(results, winslash = "/", mustWork = FALSE))
}

reset_directory <- function(path) {
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE, force = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

copy_assets <- function(files, chapter_key, asset_type, chapter_asset_root, dest_dir, repo_root, quiet, timestamp) {
  if (!length(files)) {
    return(list())
  }
  entries <- list()
  chapter_asset_root <- normalizePath(chapter_asset_root, winslash = "/", mustWork = FALSE)
  dest_dir <- normalizePath(dest_dir, winslash = "/", mustWork = FALSE)
  for (src in files) {
    normalized_src <- normalizePath(src, winslash = "/", mustWork = TRUE)
    rel_within <- sub(paste0("^", chapter_asset_root, "/"), "", normalized_src)
    if (!nzchar(rel_within) || identical(rel_within, normalized_src)) {
      rel_within <- basename(normalized_src)
    }
    sanitized <- sanitize_component(rel_within)
    dest_path <- file.path(dest_dir, paste(chapter_key, sanitized, sep = "__"))
    dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
    if (!quiet) {
      message(sprintf("[pack-thesis] %s -> %s", relative_path(normalized_src, repo_root), relative_path(dest_path, repo_root)))
    }
    if (!file.copy(normalized_src, dest_path, overwrite = TRUE)) {
      stop(sprintf("Failed to copy %s to %s", normalized_src, dest_path))
    }
    size_bytes <- unname(file.info(dest_path)$size)
    if (is.na(size_bytes)) {
      size_bytes <- 0
    }
    entries[[length(entries) + 1L]] <- list(
      chapter = chapter_key,
      asset_type = asset_type,
      source = list(
        absolute = normalized_src,
        relative = relative_path(normalized_src, repo_root),
        within_chapter = file.path(asset_type, rel_within)
      ),
      destination = list(
        absolute = normalizePath(dest_path, winslash = "/", mustWork = TRUE),
        relative = relative_path(dest_path, repo_root),
        filename = basename(dest_path)
      ),
      size_bytes = size_bytes,
      md5 = unname(tools::md5sum(dest_path)),
      collected_at = timestamp
    )
  }
  entries
}

write_manifest <- function(entries, manifest_path, asset_type, pack_cfg, config_bundle, repo_root, timestamp) {
  ensure_packages("jsonlite")
  totals <- list(
    files = length(entries),
    size_bytes = if (length(entries)) sum(vapply(entries, function(x) x$size_bytes, numeric(1)), na.rm = TRUE) else 0
  )
  manifest <- list(
    generated_at = timestamp,
    asset_type = asset_type,
    config = list(
      config_path = relative_path(config_bundle$config_path, repo_root),
      include_chapters = pack_cfg$include_chapters
    ),
    destinations = list(
      aggregate_root = pack_cfg$aggregate_root,
      target_dir = if (asset_type == "tables") pack_cfg$tables_dir else pack_cfg$figures_dir
    ),
    totals = totals,
    assets = entries
  )
  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)
  manifest
}

write_summary_manifest <- function(tables_manifest_path, figures_manifest_path, tables_manifest, figures_manifest, summary_path, repo_root, timestamp) {
  ensure_packages("jsonlite")
  summary <- list(
    generated_at = timestamp,
    manifests = list(
      tables = list(
        path = relative_path(tables_manifest_path, repo_root),
        totals = tables_manifest$totals
      ),
      figures = list(
        path = relative_path(figures_manifest_path, repo_root),
        totals = figures_manifest$totals
      )
    ),
    totals = list(
      files = tables_manifest$totals$files + figures_manifest$totals$files,
      size_bytes = tables_manifest$totals$size_bytes + figures_manifest$totals$size_bytes
    )
  )
  dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(summary, summary_path, auto_unbox = TRUE, pretty = TRUE)
}

main <- function() {
  args <- parse_arguments(commandArgs(trailingOnly = TRUE))
  repo_root <- find_repo_root()
  source_config_loader(repo_root)

  config_path <- args$config
  if (is.null(config_path)) {
    config_path <- file.path(repo_root, "reproduced", "config", "thesis.yml")
  }
  config_bundle <- load_configuration(config_path)
  pack_cfg <- get_config_value(config_bundle, "thesis", "packaging", required = TRUE)

  aggregate_root <- resolve_repo_path(config_bundle, pack_cfg$aggregate_root, must_exist = FALSE)
  tables_dir <- resolve_repo_path(config_bundle, pack_cfg$tables_dir, must_exist = FALSE)
  figures_dir <- resolve_repo_path(config_bundle, pack_cfg$figures_dir, must_exist = FALSE)
  manifests_dir <- resolve_repo_path(config_bundle, pack_cfg$manifests_dir, must_exist = FALSE)
  tables_manifest_path <- resolve_repo_path(config_bundle, pack_cfg$tables_manifest, must_exist = FALSE)
  figures_manifest_path <- resolve_repo_path(config_bundle, pack_cfg$figures_manifest, must_exist = FALSE)
  summary_manifest_path <- resolve_repo_path(config_bundle, pack_cfg$summary_manifest, must_exist = FALSE)

  dir.create(aggregate_root, recursive = TRUE, showWarnings = FALSE)
  reset_directory(tables_dir)
  reset_directory(figures_dir)
  dir.create(manifests_dir, recursive = TRUE, showWarnings = FALSE)

  timestamp <- format_timestamp()

  chapters <- pack_cfg$include_chapters
  patterns <- pack_cfg$asset_patterns

  table_entries <- list()
  figure_entries <- list()

  for (chapter_key in chapters) {
    ensure_chapter_enabled(config_bundle, chapter_key)
    chapter_paths <- resolve_chapter_output_paths(config_bundle, chapter_key, create = FALSE)
    chapter_tables_dir <- chapter_paths[["tables"]]
    chapter_figures_dir <- chapter_paths[["figures"]]

    chapter_tables <- collect_assets(chapter_tables_dir, patterns$tables)
    if (length(chapter_tables)) {
      table_entries <- c(
        table_entries,
        copy_assets(chapter_tables, chapter_key, "tables", chapter_tables_dir, tables_dir, repo_root, args$quiet, timestamp)
      )
    }

    chapter_figures <- collect_assets(chapter_figures_dir, patterns$figures)
    if (length(chapter_figures)) {
      figure_entries <- c(
        figure_entries,
        copy_assets(chapter_figures, chapter_key, "figures", chapter_figures_dir, figures_dir, repo_root, args$quiet, timestamp)
      )
    }
  }

  tables_manifest <- write_manifest(table_entries, tables_manifest_path, "tables", pack_cfg, config_bundle, repo_root, timestamp)
  figures_manifest <- write_manifest(figure_entries, figures_manifest_path, "figures", pack_cfg, config_bundle, repo_root, timestamp)
  write_summary_manifest(tables_manifest_path, figures_manifest_path, tables_manifest, figures_manifest, summary_manifest_path, repo_root, timestamp)

  message(
    sprintf(
      "[pack-thesis] Packaged %d tables and %d figures into %s",
      length(table_entries),
      length(figure_entries),
      relative_path(aggregate_root, repo_root)
    )
  )
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("[pack-thesis] Error: ", e$message)
      quit(status = 1L)
    }
  )
}
