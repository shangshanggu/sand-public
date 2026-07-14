#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(yaml)
  library(readr)
  library(jsonlite)
})

script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grep(file_arg, args)])
  if (length(script_path) == 0) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  normalizePath(dirname(script_path[1]), winslash = "/", mustWork = TRUE)
}

# Resolve repo root: when --file= is available, script lives 4 levels below
# the repo root.  When running under conda run (no --file=), script_dir()
# returns getwd() which is already reproduced/, so we only need to go 1 level
# up to reach the workspace root.
.resolve_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  has_file_arg <- any(grepl("^--file=", args))
  if (has_file_arg) {
    # script path: reproduced/analyses/chapter8_interventions/scripts/ -> 4 up
    return(normalizePath(file.path(script_dir(), "..", "..", "..", ".."),
                         winslash = "/", mustWork = TRUE))
  }
  # conda run / direct Rscript: cwd is typically reproduced/
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  # Check whether cwd already looks like the repo root (has reproduced/ child)
  if (dir.exists(file.path(cwd, "reproduced"))) return(cwd)
  # Otherwise cwd is reproduced/ itself — go one level up
  normalizePath(file.path(cwd, ".."), winslash = "/", mustWork = TRUE)
}
repo_root <- .resolve_repo_root()
source(file.path(repo_root, "reproduced", "R", "common.R"))

`%||%` <- function(x, y) {
  if (is.null(x) || (is.character(x) && length(x) == 1L && !nzchar(x))) y else x
}

stop_bad <- function(msg) {
  stop(msg, call. = FALSE)
}

as_int <- function(x, default = 0L) {
  out <- suppressWarnings(as.integer(x))
  out[is.na(out)] <- default
  out
}

default_scenario_id <- function(row) {
  targeting <- gsub("-", "_", tolower(row$intervention_targeting))
  sprintf("type_%s_wave%s_%s", tolower(row$intervention_type), row$intervention_wave, targeting)
}

read_strategy_csv <- function(path) {
  if (!file.exists(path)) {
    stop_bad(sprintf("Strategy CSV not found: %s", path))
  }
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
  required <- c("intervention_type", "intervention_wave", "intervention_targeting")
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop_bad(sprintf("Strategy CSV %s missing required columns: %s", path, paste(missing, collapse = ", ")))
  }
  df$intervention_wave <- as_int(df$intervention_wave, default = NA_integer_)
  if (any(is.na(df$intervention_wave))) {
    stop_bad("Strategy CSV has invalid intervention_wave values")
  }
  df
}

build_manifest <- function(config_path, output_path) {
  cfg <- yaml::read_yaml(config_path)
  if (!is.list(cfg)) stop_bad("Configuration root must be a mapping")

  strategy_csv_value <- cfg$strategy_csv
  if (!is.character(strategy_csv_value) || !nzchar(strategy_csv_value)) {
    stop_bad("Configuration must include 'strategy_csv' pointing to the CSV grid")
  }
  # strategy_csv in interventions.yml is relative to reproduced/ (e.g. "config/scenarios/...")
  strategy_repro <- file.path(repo_root, "reproduced", strategy_csv_value)
  # Fallback: try relative to the config file's own directory
  strategy_local <- file.path(dirname(config_path), basename(strategy_csv_value))
  strategy_csv_path <- if (file.exists(strategy_repro)) strategy_repro else strategy_local
  strategy_csv_path <- normalizePath(strategy_csv_path, winslash = "/", mustWork = TRUE)
  strategy_rows <- read_strategy_csv(strategy_csv_path)

  defaults <- cfg$defaults %||% list()
  if (!is.list(defaults)) stop_bad("'defaults' section must be a mapping")
  seeds <- defaults$seeds %||% list()
  if (!is.list(seeds)) stop_bad("'defaults.seeds' must be a mapping")
  base_seed <- as_int(seeds$base, default = 0L)
  increment <- as_int(seeds$increment, default = 1L)

  overrides <- cfg$overrides %||% list()
  if (!is.list(overrides)) stop_bad("'overrides' section must be a mapping")

  manifest <- list(
    generated_at = format_timestamp(),
    config_path = relative_repo_path(config_path, repo_root),
    strategy_csv = relative_repo_path(strategy_csv_path, repo_root),
    defaults = defaults[setdiff(names(defaults), "seeds")],
    seed_policy = list(base = base_seed, increment = increment),
    scenarios = list()
  )

  for (i in seq_len(nrow(strategy_rows))) {
    row <- strategy_rows[i, ]
    key <- sprintf("%s-%s-%s", row$intervention_type, row$intervention_wave, row$intervention_targeting)
    override <- overrides[[key]] %||% list()
    if (!is.list(override)) stop_bad(sprintf("Override for %s must be a mapping", key))

    scenario_id <- override$id %||% default_scenario_id(row)
    description <- override$description %||% defaults$description %||% ""
    tags <- override$tags %||% list()
    if (is.atomic(tags)) tags <- as.list(tags)
    if (!is.list(tags)) stop_bad(sprintf("Override tags for %s must be a list", key))

    seed_offset <- as_int(override$seed_offset, default = 0L)
    scenario_seed <- base_seed + (i - 1L) * increment + seed_offset

    manifest$scenarios[[length(manifest$scenarios) + 1L]] <- list(
      key = key,
      id = scenario_id,
      intervention_type = row$intervention_type,
      intervention_wave = row$intervention_wave,
      intervention_targeting = row$intervention_targeting,
      seed = scenario_seed,
      seed_components = list(
        base = base_seed,
        index_contribution = (i - 1L) * increment,
        offset = seed_offset
      ),
      input_proportions = defaults$input_proportions %||% list(),
      input_efficacies = defaults$input_efficacies %||% list(),
      iterations = defaults$iterations,
      description = description,
      tags = tags
    )
  }

  ensure_parent_dir(output_path)
  jsonlite::write_json(manifest, output_path, auto_unbox = TRUE, pretty = TRUE)
  manifest
}

cli_help <- function() {
  cat(
    paste(
      "Usage: 00_build_scenario_manifest.R [--config PATH] [--output PATH]",
      "",
      "Options:",
      "  --config PATH   Scenario configuration YAML (default: reproduced/config/scenarios/interventions.yml)",
      "  --output PATH   Manifest JSON output path (default: reproduced/outputs/chapter8/logs/scenario_manifest.json)",
      "  --help          Show this message and exit",
      sep = "\n"
    )
  )
}

parse_args <- function(args) {
  defaults <- list(
    config = file.path(repo_root, "reproduced", "config", "scenarios", "interventions.yml"),
    output = file.path(repo_root, "reproduced", "outputs", "chapter8", "logs", "scenario_manifest.json")
  )
  if (!length(args)) return(defaults)

  out <- defaults
  i <- 1L
  while (i <= length(args)) {
    flag <- args[[i]]
    if (flag == "--config" && i < length(args)) {
      out$config <- normalizePath(args[[i + 1]], winslash = "/", mustWork = FALSE)
      i <- i + 2L
      next
    }
    if (flag == "--output" && i < length(args)) {
      out$output <- normalizePath(args[[i + 1]], winslash = "/", mustWork = FALSE)
      i <- i + 2L
      next
    }
    if (flag %in% c("-h", "--help")) {
      cli_help()
      quit(status = 0L, save = "no")
    }
    stop_bad(sprintf("Unknown option '%s'", flag))
  }
  out
}

run <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  ensure_jsonlite("Required to write manifest output")

  config_path <- normalizePath(args$config, winslash = "/", mustWork = TRUE)
  output_path <- normalizePath(args$output, winslash = "/", mustWork = FALSE)

  manifest <- build_manifest(config_path, output_path)
  scenario_count <- length(manifest$scenarios %||% list())

  thesis_cfg <- yaml::read_yaml(file.path(repo_root, "reproduced", "config", "thesis.yml"))
  logs_root <- file.path(repo_root, thesis_cfg$project$paths$logs_dir %||% "reproduced/logs")

  entry <- list(
    timestamp = format_timestamp(),
    action = "build_manifest",
    config = relative_repo_path(config_path, repo_root),
    output = relative_repo_path(output_path, repo_root),
    scenarios = scenario_count
  )
  append_pipeline_log(logs_root, "chapter8", entry, history_key = "manifest_builds")

  message(sprintf("Built manifest with %d scenarios \u2192 %s", scenario_count, output_path))
}

run()
