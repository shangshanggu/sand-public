# ==============================================================================
# SHARED VISUALISATION UTILITIES
# ==============================================================================
#
# Shared helper functions for Chapter 7 visualisation scripts:
#   - 03_extract_chains.R   (microstep chain extraction)
#   - 04_animate_microsteps.R (networkDynamic animation)
#   - 05_interactive_networks.R (visNetwork HTML panels)
#   - plot_network_panel.R  (static network panels)
#
# Usage:
#   source("reproduced/scripts/visualisation/viz_utils.R")
#
# Requirements: 5.1, 5.3, 5.6, 6.1, 6.2
# ==============================================================================

# --- Package checking --------------------------------------------------------

#' Check that required R packages are installed, stop with install instructions
#' if any are missing.
#'
#' @param packages Character vector of package names to check.
#' @param context  Optional string describing which script needs them (for the
#'                 error message).
#' @return Invisible TRUE if all packages are available.
check_packages <- function(packages, context = NULL) {
  packages <- unique(packages)
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0L) return(invisible(TRUE))

  install_cmd <- sprintf(
    "install.packages(c(%s))",
    paste(sprintf('"%s"', missing), collapse = ", ")
  )
  ctx_msg <- if (!is.null(context) && nzchar(context)) {
    paste0(" (required by ", context, ")")
  } else {
    ""
  }
  stop(
    sprintf(
      "Missing required R package%s%s: %s\n  Install with:\n    R -e '%s'",
      if (length(missing) > 1L) "s" else "",
      ctx_msg,
      paste(missing, collapse = ", "),
      install_cmd
    ),
    call. = FALSE
  )
}

# --- Path resolution ---------------------------------------------------------

#' Resolve the repository root directory.
#'
#' When run via `Rscript --file=...`, the root is derived from the script
#' location (two levels up from `reproduced/scripts/visualisation/`).
#' Otherwise falls back to checking the working directory.
#'
#' @return Absolute path to the `reproduced/` directory.
resolve_repo_root <- function() {
  # 1. Try --file= argument (Rscript invocation)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("--file=", "", args[grepl("--file=", args)])
  if (length(file_arg) > 0) {
    return(normalizePath(file.path(dirname(file_arg), "..", ".."), mustWork = TRUE))
  }
  # 2. Working directory is reproduced/

if (file.exists("config/thesis.yml")) return(normalizePath("."))
  # 3. Working directory is repo root (one level above reproduced/)
  if (file.exists("reproduced/config/thesis.yml")) return(normalizePath("reproduced"))
  stop("Cannot resolve repo root. Run from the repository root or reproduced/ directory.")
}

#' Resolve the active data directory (real or proxy).
#'
#' Precedence:
#'   1. `SAND_DATA_MODE` environment variable
#'   2. `data.mode` key in thesis.yml
#'   3. Default: real data (`reproduced/data/raw/`)
#'
#' @param repo_root Absolute path to the `reproduced/` directory (from
#'                  `resolve_repo_root()`).
#' @param config    Optional parsed thesis.yml list. If NULL the function reads
#'                  thesis.yml from `repo_root/config/thesis.yml`.
#' @return Absolute path to the data directory.
resolve_data_dir <- function(repo_root, config = NULL) {
  # Read config if not supplied
  if (is.null(config)) {
    yml_path <- file.path(repo_root, "config", "thesis.yml")
    if (file.exists(yml_path)) {
      check_packages("yaml", context = "resolve_data_dir")
      config <- yaml::read_yaml(yml_path)
    }
  }

  # Determine mode: env var > config > default
  env_mode <- Sys.getenv("SAND_DATA_MODE", "")
  if (nzchar(env_mode)) {
    mode <- tolower(env_mode)
  } else if (!is.null(config) && !is.null(config$data) && !is.null(config$data$mode)) {
    mode <- tolower(config$data$mode)
  } else {
    mode <- "real"
  }

  if (!mode %in% c("real", "proxy")) {
    stop(sprintf("Unsupported data mode '%s'. Use 'real' or 'proxy'.", mode), call. = FALSE)
  }

  if (identical(mode, "proxy")) {
    # Use proxy_dir from config, or fall back to default
    proxy_rel <- if (!is.null(config) && !is.null(config$data) && !is.null(config$data$proxy_dir)) {
      config$data$proxy_dir
    } else {
      "reproduced/data/proxy"
    }
    # proxy_dir in thesis.yml is repo-root-relative (e.g. "reproduced/data/proxy")
    # but we already resolved to the reproduced/ directory, so strip the prefix
    proxy_rel <- sub("^reproduced/", "", proxy_rel)
    data_dir <- file.path(repo_root, proxy_rel)
  } else {
    data_dir <- file.path(repo_root, "data", "raw")
  }

  normalizePath(data_dir, mustWork = FALSE)
}

# --- Data loading ------------------------------------------------------------

#' Load the list_by_wave object from an RData file.
#'
#' @param data_dir Path to the data directory containing `list_by_wave.RData`.
#' @return The `list_by_wave` list object.
load_list_by_wave <- function(data_dir) {
  path <- file.path(data_dir, "list_by_wave.RData")
  if (!file.exists(path)) {
    stop("list_by_wave.RData not found at: ", path, call. = FALSE)
  }
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  if (!"list_by_wave" %in% ls(env)) {
    stop("list_by_wave object not found in RData file", call. = FALSE)
  }
  get("list_by_wave", envir = env)
}

# --- AUDIT-C colour palette --------------------------------------------------

#' Generate a muted academic colour gradient for AUDIT-C scores.
#'
#' Maps scores from 0 (soft teal / low risk) through warm sand (moderate) to
#' deep wine (high risk). Colourblind-safe, inspired by Brewer sequential
#' palettes used in epidemiology publications.
#'
#' @param scores    Numeric vector of AUDIT-C scores.
#' @param max_score Maximum possible score (default 12).
#' @param format    "hex" returns #RRGGBB strings (for ndtv/base R),
#'                  "rgba" returns rgba() CSS strings (for vis.js/HTML).
#' @return Character vector of colour strings.
audit_palette <- function(scores, max_score = 12, format = "hex") {
  scores[is.na(scores)] <- 0
  scores <- pmin(scores, max_score)
  frac <- scores / max_score

  # Five-stop gradient: soft teal → sage → warm sand → terracotta → deep wine
  stops <- data.frame(
    f = c(0.00,  0.25,  0.50,  0.75,  1.00),
    r = c(0.576, 0.608, 0.816, 0.776, 0.545),
    g = c(0.749, 0.718, 0.706, 0.490, 0.271),
    b = c(0.710, 0.620, 0.498, 0.357, 0.306)
  )

  interp <- function(val, channel) {
    idx <- findInterval(val, stops$f, all.inside = TRUE)
    lo <- stops$f[idx]; hi <- stops$f[idx + 1]
    t <- (val - lo) / (hi - lo)
    stops[[channel]][idx] * (1 - t) + stops[[channel]][idx + 1] * t
  }

  rv <- vapply(frac, interp, numeric(1), channel = "r")
  gv <- vapply(frac, interp, numeric(1), channel = "g")
  bv <- vapply(frac, interp, numeric(1), channel = "b")

  if (identical(format, "rgba")) {
    sprintf("rgba(%d,%d,%d,0.88)", round(rv * 255), round(gv * 255), round(bv * 255))
  } else {
    rgb(rv, gv, bv)
  }
}

#' Generate a slightly darker border colour for AUDIT-C node borders.
#'
#' Same gradient as audit_palette() but darkened by 30%% for contrast.
#'
#' @param scores    Numeric vector of AUDIT-C scores.
#' @param max_score Maximum possible score (default 12).
#' @param format    "hex" or "rgba" (see audit_palette).
#' @return Character vector of colour strings.
audit_border_palette <- function(scores, max_score = 12, format = "hex") {
  scores[is.na(scores)] <- 0
  scores <- pmin(scores, max_score)
  frac <- scores / max_score

  stops <- data.frame(
    f = c(0.00,  0.25,  0.50,  0.75,  1.00),
    r = c(0.576, 0.608, 0.816, 0.776, 0.545),
    g = c(0.749, 0.718, 0.706, 0.490, 0.271),
    b = c(0.710, 0.620, 0.498, 0.357, 0.306)
  )

  interp <- function(val, channel) {
    idx <- findInterval(val, stops$f, all.inside = TRUE)
    lo <- stops$f[idx]; hi <- stops$f[idx + 1]
    t <- (val - lo) / (hi - lo)
    stops[[channel]][idx] * (1 - t) + stops[[channel]][idx + 1] * t
  }

  rv <- vapply(frac, interp, numeric(1), channel = "r") * 0.7
  gv <- vapply(frac, interp, numeric(1), channel = "g") * 0.7
  bv <- vapply(frac, interp, numeric(1), channel = "b") * 0.7

  if (identical(format, "rgba")) {
    sprintf("rgba(%d,%d,%d,0.95)", round(rv * 255), round(gv * 255), round(bv * 255))
  } else {
    rgb(rv, gv, bv)
  }
}

# --- Configuration -----------------------------------------------------------

#' Read the `visualisation` block from thesis.yml with sensible defaults.
#'
#' Returns a nested list with all visualisation parameters. Missing keys are
#' filled in from hardcoded defaults so callers never need to check for NULL.
#'
#' @param repo_root Absolute path to the `reproduced/` directory.
#' @param config    Optional pre-parsed thesis.yml list. If NULL the function
#'                  reads thesis.yml itself.
#' @return A list with `$microstep` and `$interactive_network` sub-lists.
read_viz_config <- function(repo_root, config = NULL) {
  # Defaults matching the design document
  defaults <- list(
    microstep = list(
      n_chains      = 5L,
      chain_index   = 1L,
      fps           = 2L,
      node_scale    = 1.5,
      output_format = "html"
    ),
    interactive_network = list(
      physics        = TRUE,
      node_scale     = 1.0,
      wave_captions  = list()
    )
  )

  # Read config if not supplied
  if (is.null(config)) {
    yml_path <- file.path(repo_root, "config", "thesis.yml")
    if (file.exists(yml_path)) {
      check_packages("yaml", context = "read_viz_config")
      config <- yaml::read_yaml(yml_path)
    }
  }

  # Extract the visualisation block (may be NULL if not yet added)
  viz <- if (!is.null(config)) config$visualisation else NULL

  # Merge user values over defaults for each sub-block
  result <- defaults
  if (!is.null(viz)) {
    for (block_name in names(defaults)) {
      if (!is.null(viz[[block_name]])) {
        for (key in names(defaults[[block_name]])) {
          if (!is.null(viz[[block_name]][[key]])) {
            result[[block_name]][[key]] <- viz[[block_name]][[key]]
          }
        }
      }
    }
  }

  result
}
