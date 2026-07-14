#!/usr/bin/env Rscript

ensure_config_packages <- function(packages, context = NULL) {
  packages <- unique(packages)
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) {
    return(invisible(TRUE))
  }

  install_hint <- sprintf(
    "Install dependencies with `make env-renv` or `R -e \"install.packages(c(%s))\"`.",
    paste(sprintf("'%s'", missing), collapse = ", ")
  )
  detail <- if (!is.null(context) && nzchar(context)) paste0("\nContext: ", context) else ""
  stop(
    sprintf(
      "Missing required R packages: %s. %s%s",
      paste(missing, collapse = ", "),
      install_hint,
      detail
    )
  )
}

resolve_existing_path <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    stop("Configuration path must be a non-empty string.")
  }

  is_absolute <- grepl("^/|^[A-Za-z]:[/\\]", path)
  candidates <- if (is_absolute) {
    path
  } else {
    unique(
      c(
        path,
        file.path("reproduced", path),
        file.path("..", path),
        file.path("..", "reproduced", path)
      )
    )
  }

  for (candidate in candidates) {
    if (nzchar(candidate) && file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop(sprintf("Unable to locate configuration file at '%s'.", path))
}

load_configuration <- function(config_path = "reproduced/config/thesis.yml") {
  ensure_config_packages(c("yaml", "jsonlite"), "Needed to load and process the thesis configuration.")
  config_path <- resolve_existing_path(config_path)
  config <- yaml::read_yaml(config_path)
  repo_root <- normalizePath(file.path(dirname(config_path), "..", ".."), winslash = "/", mustWork = TRUE)
  structure(
    list(
      config = config,
      repo_root = repo_root,
      config_path = config_path
    ),
    class = "thesis_config"
  )
}

resolve_repo_path <- function(config_bundle, path, must_exist = FALSE) {
  if (!inherits(config_bundle, "thesis_config")) {
    stop("`config_bundle` must be created by load_configuration().")
  }
  if (is.null(path) || !nzchar(path)) {
    stop("Path entry missing from configuration.")
  }
  normalizePath(
    file.path(config_bundle$repo_root, path),
    winslash = "/",
    mustWork = isTRUE(must_exist)
  )
}

resolve_data_dir <- function(config_bundle, mode_override = NULL, must_exist = FALSE) {
  if (!inherits(config_bundle, "thesis_config")) {
    stop("`config_bundle` must be created by load_configuration().")
  }

  env_mode <- Sys.getenv("SAND_DATA_MODE", "")
  mode <- if (!is.null(mode_override) && nzchar(mode_override)) {
    mode_override
  } else if (nzchar(env_mode)) {
    env_mode
  } else {
    get_config_value(config_bundle, "data", "mode", default = "real")
  }

  mode <- tolower(as.character(mode))
  if (!mode %in% c("real", "proxy")) {
    stop(sprintf("Unsupported data.mode '%s'. Use 'real' or 'proxy'.", mode))
  }

  if (identical(mode, "proxy")) {
    proxy_dir <- get_config_value(config_bundle, "data", "proxy_dir", default = NULL)
    if (is.null(proxy_dir) || !nzchar(proxy_dir)) {
      stop("data.proxy_dir must be set when data.mode is 'proxy'.")
    }
    return(resolve_repo_path(config_bundle, proxy_dir, must_exist = must_exist))
  }

  raw_dir <- get_config_value(config_bundle, "project", "paths", "raw_data_dir", required = TRUE)
  resolve_repo_path(config_bundle, raw_dir, must_exist = must_exist)
}

resolve_chapter_output_paths <- function(config_bundle, chapter_key, create = TRUE) {
  if (!inherits(config_bundle, "thesis_config")) {
    stop("`config_bundle` must be created by load_configuration().")
  }
  if (is.null(chapter_key) || !nzchar(chapter_key)) {
    stop("`chapter_key` must be a non-empty character scalar.")
  }

  outputs_rel <- get_config_value(config_bundle, "chapters", chapter_key, "outputs_dir", required = TRUE)
  base_dir <- resolve_repo_path(config_bundle, outputs_rel, must_exist = FALSE)

  subdirs <- list(
    base = base_dir,
    manifests = file.path(base_dir, "manifests"),
    tables = file.path(base_dir, "tables"),
    figures = file.path(base_dir, "figures"),
    logs = file.path(base_dir, "logs"),
    data = file.path(base_dir, "data")
  )

  if (isTRUE(create)) {
    for (dir_path in unique(unlist(subdirs, use.names = FALSE))) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
  }

  subdirs
}

get_config_value <- function(config_bundle, ..., default = NULL, required = FALSE) {
  if (!inherits(config_bundle, "thesis_config")) {
    stop("`config_bundle` must be created by load_configuration().")
  }
  keys <- list(...)
  if (length(keys) == 1 && length(keys[[1]]) == 1) {
    raw_key <- keys[[1]]
    if (is.character(raw_key) && grepl("\\.", raw_key, fixed = FALSE)) {
      keys <- strsplit(raw_key, ".", fixed = TRUE)[[1]]
    }
  }
  current <- config_bundle$config
  for (key in keys) {
    if (is.null(current) || is.null(key) || !nzchar(key)) {
      current <- NULL
      break
    }
    if (is.list(current) && !is.null(current[[key]])) {
      current <- current[[key]]
    } else {
      current <- NULL
      break
    }
  }
  if (is.null(current)) {
    if (isTRUE(required)) {
      stop(sprintf("Missing required configuration value for path: %s", paste(unlist(keys), collapse = ".")))
    }
    return(default)
  }
  current
}

ensure_chapter_enabled <- function(config_bundle, chapter_key) {
  enabled <- get_config_value(config_bundle, "chapters", chapter_key, "enabled", default = FALSE)
  if (!isTRUE(enabled)) {
    stop(sprintf("Chapter '%s' is disabled in reproduced/config/thesis.yml.", chapter_key))
  }
  invisible(TRUE)
}

print_config_value <- function(value) {
  ensure_config_packages("jsonlite", "Required to print structured configuration values.")
  if (is.atomic(value) && length(value) == 1) {
    cat(as.character(value), "\n")
  } else {
    cat(jsonlite::toJSON(value, auto_unbox = TRUE, pretty = TRUE), "\n")
  }
}

usage <- function() {
  message("Usage: Rscript reproduced/scripts/utils/config_loader.R [options]\n",
          "\n",
          "Options:\n",
          "  --config <path>             Path to config YAML (default: reproduced/config/thesis.yml)\n",
          "  --get <dot.path>            Print a configuration value using dot notation\n",
          "  --resolve <relative_path>   Resolve a repo-relative path from the configuration\n",
          "  --chapter-enabled <key>     Check that a chapter is enabled (exits non-zero if disabled)\n",
          "  --help                      Display this message\n")
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    return(invisible(TRUE))
  }
  config_path <- "reproduced/config/thesis.yml"
  op <- list()
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--config") {
      if (i == length(args)) stop("--config requires a path argument.")
      i <- i + 1
      config_path <- args[[i]]
    } else if (arg == "--get") {
      if (i == length(args)) stop("--get requires a dot path argument.")
      i <- i + 1
      op$get <- args[[i]]
    } else if (arg == "--resolve") {
      if (i == length(args)) stop("--resolve requires a relative path argument.")
      i <- i + 1
      op$resolve <- args[[i]]
    } else if (arg == "--chapter-enabled") {
      if (i == length(args)) stop("--chapter-enabled requires a chapter key argument.")
      i <- i + 1
      op$chapter <- args[[i]]
    } else if (arg %in% c("--help", "-h")) {
      usage()
      return(invisible(TRUE))
    } else {
      stop(sprintf("Unknown argument: %s", arg))
    }
    i <- i + 1
  }
  bundle <- load_configuration(config_path)
  if (!is.null(op$get)) {
    value <- get_config_value(bundle, op$get, default = NULL, required = TRUE)
    print_config_value(value)
  }
  if (!is.null(op$resolve)) {
    resolved <- resolve_repo_path(bundle, op$resolve, must_exist = FALSE)
    cat(resolved, "\n")
  }
  if (!is.null(op$chapter)) {
    ensure_chapter_enabled(bundle, op$chapter)
    cat(sprintf("Chapter %s is enabled.\n", op$chapter))
  }
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !length(sys.frames())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error: ", e$message)
      quit(status = 1)
    }
  )
}
