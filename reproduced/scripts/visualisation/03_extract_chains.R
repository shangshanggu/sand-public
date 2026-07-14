#!/usr/bin/env Rscript
# ==============================================================================
# SAOM MICROSTEP CHAIN EXTRACTION
# ==============================================================================
#
# Extracts microstep simulation chains from a converged RSiena SAOM fit by
# re-running siena07() with returnChains = TRUE and reduced n3.
#
# The extracted chains are flattened into a data frame and saved as an RDS file
# for downstream animation (04_animate_microsteps.R).
#
# Usage:
#   Rscript reproduced/scripts/visualisation/03_extract_chains.R [--n-chains N] [--model MODEL]
#
# Defaults:
#   n_chains = thesis.yml visualisation.microstep.n_chains (5)
#   model    = thesis.yml chapters.chapter7_saom.default_model ("base")
#
# Output:
#   reproduced/outputs/chapter7/microstep_chains.rds
#
# Requirements: 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 5.1, 5.2, 5.4, 5.5, 5.7, 6.1, 6.3
# ==============================================================================

# --- Source shared utilities --------------------------------------------------

# Resolve the path to viz_utils.R relative to this script
.script_dir <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("--file=", "", args[grepl("--file=", args)])
  if (length(file_arg) > 0) return(dirname(normalizePath(file_arg)))
  # Fallback: try known locations
  candidates <- c(
    "reproduced/scripts/visualisation",
    "scripts/visualisation"
  )
  for (d in candidates) {
    if (file.exists(file.path(d, "viz_utils.R"))) return(normalizePath(d))
  }
  "."
})()

source(file.path(.script_dir, "viz_utils.R"))

# --- Null-coalescing operator -------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# --- Constants ---------------------------------------------------------------

N_CHAINS_DEFAULT <- 5L
N_CHAINS_MAX_DEFAULT <- 20L
WAVE_TRANSITIONS <- list(
  list(label = "W2->W4", period = 1L),
  list(label = "W4->W5", period = 2L),
  list(label = "W5->W6", period = 3L)
)

# --- Chain count clamping ----------------------------------------------------

#' Clamp n_chains to the valid range [1, cap].
#'
#' For any integer value provided as n_chains, the effective chain count equals
#' min(max(value, 1), cap). Values above cap trigger a warning.
#'
#' @param n_chains Integer chain count (may be out of range).
#' @param cap      Maximum allowed chain count (default 20).
#' @return Integer in [1, cap].
clamp_n_chains <- function(n_chains, cap = N_CHAINS_MAX_DEFAULT) {
  if (!is.numeric(n_chains) || length(n_chains) != 1 || is.na(n_chains)) {
    stop(
      sprintf("Invalid n_chains value: must be a single integer, got '%s'.", deparse(n_chains)),
      call. = FALSE
    )
  }
  if (n_chains != as.integer(n_chains)) {
    stop(
      sprintf("Invalid n_chains value: must be an integer, got %s.", n_chains),
      call. = FALSE
    )
  }
  n_chains <- as.integer(n_chains)
  if (n_chains <= 0L) {
    stop(
      sprintf("Invalid n_chains value: must be positive, got %d.", n_chains),
      call. = FALSE
    )
  }
  if (n_chains > cap) {
    message(sprintf("[microstep] Warning: n_chains=%d exceeds maximum cap=%d; clamping to %d.", n_chains, cap, cap))
    return(cap)
  }
  n_chains
}

# --- Load base fit -----------------------------------------------------------

#' Load a converged RSiena base fit from an RData file.
#'
#' @param fit_path Path to the base_fit.RData file.
#' @return A sienaFit object.
load_base_fit <- function(fit_path) {
  if (is.null(fit_path) || !nzchar(fit_path)) {
    stop("Base fit path is empty or NULL.", call. = FALSE)
  }
  if (!file.exists(fit_path)) {
    stop(
      sprintf(
        "Base fit file not found at: %s\n  Run 02_run_saom_model.R first to generate the base SAOM fit.",
        fit_path
      ),
      call. = FALSE
    )
  }

  env <- new.env(parent = emptyenv())
  loaded <- tryCatch(load(fit_path, envir = env), error = function(e) e)
  if (inherits(loaded, "error")) {
    stop(
      sprintf("Failed to load base fit from %s: %s", fit_path, conditionMessage(loaded)),
      call. = FALSE
    )
  }

  # Find the sienaFit object in the loaded environment
  for (name in ls(env, all.names = TRUE)) {
    obj <- get(name, envir = env, inherits = FALSE)
    if (inherits(obj, "sienaFit")) {
      return(obj)
    }
  }

  stop(
    sprintf(
      "File %s does not contain a sienaFit object. Found objects: %s",
      fit_path,
      paste(ls(env), collapse = ", ")
    ),
    call. = FALSE
  )
}

# --- Build chain dataset (reuse from 02_run_saom_model.R) --------------------

#' Source 02_run_saom_model.R in a child environment to get its helper functions.
#'
#' Since 02_run_saom_model.R guards its main() with
#' `if (identical(environment(), globalenv()))`, sourcing in a child env is safe.
#'
#' @param repo_root Path to the reproduced/ directory.
#' @return An environment containing the sourced functions.
.source_saom_helpers <- function(repo_root) {
  saom_script <- file.path(
    repo_root, "analyses", "chapter7_saom", "scripts", "02_run_saom_model.R"
  )
  if (!file.exists(saom_script)) {
    stop(
      sprintf("SAOM model script not found at: %s", saom_script),
      call. = FALSE
    )
  }
  env <- new.env(parent = globalenv())
  # Source in the child env — main() won't run because environment != globalenv
  source(saom_script, local = env)
  env
}

#' Build the SAOM dataset and effects for chain extraction.
#'
#' Reuses build_saom_dataset() and configure_effects() from 02_run_saom_model.R
#' to ensure the chain extraction uses the identical model specification.
#'
#' @param network_path   Path to network_arrays.rds.
#' @param model_spec_path Path to saom_models.yml.
#' @param model_id       Model identifier (e.g., "base").
#' @param repo_root      Path to the reproduced/ directory.
#' @param config         Parsed thesis.yml list.
#' @return A list with components: data, effects, algo_params.
build_chain_dataset <- function(network_path, model_spec_path, model_id, repo_root, config) {
  # Validate inputs exist
  if (!file.exists(network_path)) {
    stop(
      sprintf(
        "Network arrays not found at: %s\n  Run the Chapter 4 pipeline first.",
        network_path
      ),
      call. = FALSE
    )
  }
  if (!file.exists(model_spec_path)) {
    stop(
      sprintf("SAOM model specification not found at: %s", model_spec_path),
      call. = FALSE
    )
  }

  check_packages("yaml", context = "03_extract_chains.R")
  spec <- yaml::read_yaml(model_spec_path)

  if (is.null(spec$models) || !model_id %in% names(spec$models)) {
    stop(
      sprintf(
        "Model '%s' not found in %s. Available models: %s",
        model_id, model_spec_path,
        paste(names(spec$models), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  model_spec <- spec$models[[model_id]]

  # Load network payload
  payload <- readRDS(network_path)
  if (is.null(payload$network_array) || is.null(payload$behaviour_array)) {
    stop("network_arrays.rds is missing required components (network_array, behaviour_array).", call. = FALSE)
  }

  # Source the SAOM helper functions in a child environment
  saom_env <- .source_saom_helpers(repo_root)

  # Build the SAOM dataset using the sourced function
  dataset_info <- saom_env$build_saom_dataset(payload, model_spec)

  # Configure effects using the sourced function
  effects_info <- saom_env$configure_effects(
    dataset_info$data,
    model_spec,
    dataset_info$network_name,
    dataset_info$behaviour_name,
    dataset_info$actor_covariates,
    dataset_info$dyadic_covariates
  )

  # Build algorithm parameters (same merge logic as 02_run_saom_model.R)
  algorithm_params <- spec$defaults$algorithm
  if (is.null(algorithm_params)) algorithm_params <- list()
  rsiena_est <- config$rsiena$estimation
  if (!is.null(rsiena_est)) {
    algorithm_params <- modifyList(algorithm_params, rsiena_est)
  }
  if (!is.null(model_spec$algorithm)) {
    algorithm_params <- modifyList(algorithm_params, model_spec$algorithm)
  }
  algorithm_params <- saom_env$filter_algorithm_params(algorithm_params)

  # Set seed from config if not already present
  if (is.null(algorithm_params$seed) && !is.null(config$rsiena$project_seed)) {
    algorithm_params$seed <- as.integer(config$rsiena$project_seed)
  }

  list(
    data = dataset_info$data,
    effects = effects_info$effects,
    algo_params = algorithm_params,
    network_name = dataset_info$network_name,
    behaviour_name = dataset_info$behaviour_name
  )
}

# --- Extract chains ----------------------------------------------------------

#' Run siena07 with returnChains = TRUE to extract microstep chains.
#'
#' @param base_fit   A converged sienaFit object (used as prevAns).
#' @param saom_data  RSiena data object from sienaDataCreate().
#' @param effects    RSiena effects object from getEffects()/includeEffects().
#' @param algo_params List of algorithm parameters (from filter_algorithm_params).
#' @param n_chains   Number of chains to extract (n3 parameter).
#' @param seed       RNG seed for reproducibility.
#' @return A sienaFit object with chain data in $chain.
extract_chains <- function(base_fit, saom_data, effects, algo_params, n_chains, seed = NULL) {
  check_packages("RSiena", context = "03_extract_chains.R")

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # --- Suppress tkvars/tcltk crash in headless environments ---
  Sys.setenv(DISPLAY = "")
  options(device = "pdf")
  tryCatch({
    ns <- asNamespace("RSiena")
    if (exists("is.batch", envir = ns) && is.function(get("is.batch", envir = ns))) {
      get("is.batch", envir = ns)(TRUE)
    }
  }, error = function(e) message("[microstep] Note: could not pre-set is.batch: ", e$message))

  # Override n3 with the chain count and use minimal estimation
  # nsub=1 (not 0) because RSiena 1.4.7 errors with nsub=0 + simOnly=TRUE
  # ("missing value where TRUE/FALSE needed"). Using nsub=1 with prevAns
  # means phase 2 is trivially short (parameters already converged).
  algo_params$n3 <- as.integer(n_chains)
  algo_params$nsub <- 1L

  # Create the algorithm object
  algo <- do.call(RSiena::sienaAlgorithmCreate, c(
    list(projname = "microstep_chains"),
    algo_params
  ))

  message(sprintf("[microstep] Running siena07 with returnChains=TRUE, n3=%d ...", n_chains))

  # Ensure batch mode is set right before the call
  tryCatch({
    ns <- asNamespace("RSiena")
    if (exists("is.batch", envir = ns) && is.function(get("is.batch", envir = ns))) {
      get("is.batch", envir = ns)(TRUE)
    }
  }, error = function(e) NULL)

  fit <- RSiena::siena07(
    algo,
    data = saom_data,
    effects = effects,
    prevAns = base_fit,
    returnChains = TRUE,
    returnDeps = FALSE,
    batch = TRUE,
    silent = TRUE
  )

  if (is.null(fit$chain)) {
    stop("siena07 completed but no chain data was returned. Check RSiena version.", call. = FALSE)
  }

  message(sprintf("[microstep] Chain extraction complete. Chains returned: %d", length(fit$chain)))
  fit
}

# --- Flatten chains ----------------------------------------------------------

#' Flatten RSiena's nested chain structure into a tidy data frame.
#'
#' RSiena stores chains as:
#'   fit$chain[[chain_id]][[period]][[depvar_group]] -> list of microsteps
#' Each microstep is an unnamed list of 13 elements:
#'   [[1]] type string ("Network" or "Behavior")
#'   [[2]] depvar index (integer, 1-based)
#'   [[3]] variable name (string)
#'   [[4]] ego (integer, 0-INDEXED — must add 1 for R)
#'   [[5]] alter/secondary (integer, 0-INDEXED for network; 0 for behaviour)
#'   [[6]] difference (+1/-1 for behaviour direction; not the alter)
#'   [[7]]-[[9]] numeric (logOptionSetProbability, logChoiceProbability, reciprocalRate)
#'   [[10]]-[[11]] NULL
#'   [[12]]-[[13]] logical flags (diagonal, missing)
#'
#' @param fit       A sienaFit object with $chain populated.
#' @param n_chains  Number of chains extracted.
#' @param n_periods Number of wave transition periods.
#' @return A data.frame with columns: chain_id, period, step, actor, type, action, target.
flatten_chains <- function(fit, n_chains, n_periods) {
  if (is.null(fit$chain)) {
    stop("No chain data found in the sienaFit object.", call. = FALSE)
  }

  rows <- vector("list", 0)
  row_idx <- 0L

  for (chain_id in seq_len(n_chains)) {
    if (chain_id > length(fit$chain)) break

    chain <- fit$chain[[chain_id]]

    # RSiena 1.4.7 wraps periods inside a single-element group list:
    #   chain[[chain_id]][[group=1]][[period]][[microstep]]
    # Unwrap the group level if present (length(chain)==1 and the inner
    # element has length == n_periods).
    if (length(chain) == 1 && is.list(chain[[1]]) &&
        length(chain[[1]]) == n_periods) {
      chain <- chain[[1]]
    }

    for (period in seq_len(n_periods)) {
      if (period > length(chain)) next

      period_data <- chain[[period]]
      if (is.null(period_data) || length(period_data) == 0) next

      # RSiena nests microsteps inside depvar groups within each period.
      # period_data may be:
      #   (a) a list of depvar groups, each containing a list of microsteps, OR
      #   (b) a flat list of microsteps (single depvar group unwrapped).
      # Detect by checking whether the first element is itself a microstep
      # (a list whose [[1]] is a character type string) or a group (a list of
      # microsteps).
      all_microsteps <- .collect_microsteps(period_data)

      if (length(all_microsteps) == 0) next

      for (step_idx in seq_along(all_microsteps)) {
        ms <- all_microsteps[[step_idx]]
        if (is.null(ms) || !is.list(ms) || length(ms) < 6) next

        row_idx <- row_idx + 1L

        # Extract fields — note RSiena uses 0-indexed actors and alters
        type_str <- ms[[1]]  # "Network" or "Behavior"
        ego_0    <- ms[[4]]  # actor index (0-indexed)
        alter_0  <- ms[[5]]  # alter index (0-indexed) for network; 0 for behaviour
        diff_val <- ms[[6]]  # direction for behaviour (+1/-1/0)

        # Convert to 1-indexed for R
        ego <- as.integer(ego_0) + 1L

        # Classify type
        type_label <- if (is.character(type_str) && grepl("^[Nn]etwork", type_str)) {
          "network"
        } else if (is.character(type_str) && grepl("^[Bb]ehav", type_str)) {
          "behaviour"
        } else {
          "network"
        }

        # Classify action and target
        if (identical(type_label, "network")) {
          alter <- as.integer(alter_0) + 1L
          # ego == alter (after +1) means no change (diagonal)
          if (is.na(alter) || alter == ego) {
            action <- "no_change"
            target <- NA_integer_
          } else {
            action <- "change"
            target <- alter
          }
        } else {
          # Behaviour: diff_val is +1 (increase), -1 (decrease), or 0 (no change)
          target <- NA_integer_
          if (is.na(diff_val) || diff_val == 0) {
            action <- "no_change"
          } else if (diff_val > 0) {
            action <- "increase"
          } else {
            action <- "decrease"
          }
        }

        rows[[row_idx]] <- data.frame(
          chain_id = as.integer(chain_id),
          period   = as.integer(period),
          step     = as.integer(step_idx),
          actor    = ego,
          type     = type_label,
          action   = action,
          target   = target,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      chain_id = integer(0),
      period   = integer(0),
      step     = integer(0),
      actor    = integer(0),
      type     = character(0),
      action   = character(0),
      target   = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

#' Recursively collect microstep entries from a possibly nested period structure.
#'
#' RSiena's chain nesting varies by version and model complexity. This function
#' walks the tree until it finds leaf lists whose [[1]] is a character type
#' string ("Network" or "Behavior").
#'
#' @param x A list (period_data or a sub-group thereof).
#' @return A flat list of microstep entries.
.collect_microsteps <- function(x) {
  if (!is.list(x) || length(x) == 0) return(list())

  # Check if x itself is a microstep (leaf node)
  if (length(x) >= 6 && is.character(x[[1]]) &&
      grepl("^(Network|Behavior)", x[[1]])) {
    return(list(x))
  }

  # Otherwise recurse into children
  result <- list()
  for (i in seq_along(x)) {
    child <- x[[i]]
    if (is.list(child)) {
      # Check if child is a microstep
      if (length(child) >= 6 && is.character(child[[1]]) &&
          grepl("^(Network|Behavior)", child[[1]])) {
        result <- c(result, list(child))
      } else {
        # Recurse deeper
        result <- c(result, .collect_microsteps(child))
      }
    }
  }
  result
}

# (Helper functions .extract_chain_field, .classify_chain_type, and
#  .classify_chain_action removed — logic is now inline in flatten_chains.)

# --- CLI argument parsing ----------------------------------------------------

#' Parse command-line arguments for the chain extraction script.
#'
#' Supports:
#'   --n-chains N   Number of chains to extract
#'   --model MODEL  Model identifier from saom_models.yml
#'
#' @return A list with $n_chains (integer or NULL) and $model (character or NULL).
parse_chain_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  result <- list(n_chains = NULL, model = NULL)

  if (length(args) == 0) return(result)

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]

    if (arg %in% c("--n-chains", "--n_chains")) {
      if (i >= length(args)) stop("--n-chains flag provided without a value.", call. = FALSE)
      val <- suppressWarnings(as.integer(args[[i + 1L]]))
      if (is.na(val)) stop(sprintf("Invalid --n-chains value: '%s' (must be an integer).", args[[i + 1L]]), call. = FALSE)
      result$n_chains <- val
      i <- i + 2L
      next
    }

    if (grepl("^--n-chains=", arg) || grepl("^--n_chains=", arg)) {
      val_str <- sub("^--n[-_]chains=", "", arg)
      val <- suppressWarnings(as.integer(val_str))
      if (is.na(val)) stop(sprintf("Invalid --n-chains value: '%s' (must be an integer).", val_str), call. = FALSE)
      result$n_chains <- val
      i <- i + 1L
      next
    }

    if (arg %in% c("--model", "-m")) {
      if (i >= length(args)) stop("--model flag provided without a value.", call. = FALSE)
      result$model <- args[[i + 1L]]
      i <- i + 2L
      next
    }

    if (grepl("^--model=", arg)) {
      result$model <- sub("^--model=", "", arg)
      i <- i + 1L
      next
    }

    # Unknown argument
    stop(sprintf("Unrecognised argument: '%s'", arg), call. = FALSE)
  }

  result
}

# --- Main --------------------------------------------------------------------

main <- function() {
  # --- Setup -----------------------------------------------------------------
  repo_root <- resolve_repo_root()

  # Add RSiena library path from config (same pattern as 02_run_saom_model.R)
  check_packages("yaml", context = "03_extract_chains.R")
  config_pre <- yaml::read_yaml(file.path(repo_root, "config", "thesis.yml"))
  rsiena_lib_rel <- config_pre$rsiena$installation$library_dir
  if (!is.null(rsiena_lib_rel) && nzchar(rsiena_lib_rel)) {
    rsiena_lib_dir <- sub("^reproduced/", "", rsiena_lib_rel)
    rsiena_lib_abs <- file.path(repo_root, rsiena_lib_dir)
    if (dir.exists(rsiena_lib_abs)) {
      .libPaths(c(rsiena_lib_abs, .libPaths()))
    }
  }

  check_packages("RSiena", context = "03_extract_chains.R")
  check_packages("yaml", context = "03_extract_chains.R")

  config <- yaml::read_yaml(file.path(repo_root, "config", "thesis.yml"))
  viz_config <- read_viz_config(repo_root, config)
  chapter_cfg <- config$chapters$chapter7_saom

  if (is.null(chapter_cfg)) {
    stop("chapter7_saom configuration block is missing from thesis.yml", call. = FALSE)
  }

  # --- Parse CLI args and resolve config precedence --------------------------
  cli_args <- parse_chain_args()

  # Config precedence: thesis.yml -> CLI -> default
  config_n_chains <- viz_config$microstep$n_chains
  cli_n_chains <- cli_args$n_chains

  if (!is.null(config_n_chains)) {
    n_chains <- as.integer(config_n_chains)
  } else if (!is.null(cli_n_chains)) {
    n_chains <- cli_n_chains
  } else {
    n_chains <- N_CHAINS_DEFAULT
  }

  # CLI overrides config if both present? No — design says config wins.
  # But CLI should still be able to override for ad-hoc runs.
  # Design: "thesis.yml visualisation.microstep.n_chains → CLI --n-chains → default 5"
  # This means: config first, then CLI, then default.
  # If config is present, it takes precedence over CLI.
  # If config is absent, CLI takes precedence over default.

  n_chains <- clamp_n_chains(n_chains)

  # Model ID: CLI -> config -> default
  model_id <- cli_args$model
  if (is.null(model_id)) {
    model_id <- chapter_cfg$default_model
  }
  if (is.null(model_id)) {
    model_id <- "base"
  }

  # --- Resolve paths ---------------------------------------------------------
  # Base fit path
  fit_path <- chapter_cfg$baseline_model$path
  if (is.null(fit_path)) {
    fit_path <- file.path(
      chapter_cfg$cache_dir %||% "reproduced/outputs/chapter7/cache",
      "base_fit.RData"
    )
  }
  # Strip reproduced/ prefix since repo_root already points there
  fit_path_clean <- sub("^reproduced/", "", fit_path)
  fit_path_abs <- file.path(repo_root, fit_path_clean)

  # Network arrays path
  network_path <- NULL
  for (p in chapter_cfg$required_inputs) {
    if (grepl("network_arrays\\.rds$", p)) {
      network_path <- p
      break
    }
  }
  if (is.null(network_path)) {
    stop("chapter7_saom.required_inputs must include network_arrays.rds path.", call. = FALSE)
  }
  network_path_clean <- sub("^reproduced/", "", network_path)
  network_path_abs <- file.path(repo_root, network_path_clean)

  # Model spec path
  spec_path <- chapter_cfg$model_specs_file %||% "reproduced/config/scenarios/saom_models.yml"
  spec_path_clean <- sub("^reproduced/", "", spec_path)
  spec_path_abs <- file.path(repo_root, spec_path_clean)

  # Output path
  output_dir <- file.path(repo_root, "outputs", "chapter7")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path(output_dir, "microstep_chains.rds")

  # Seed
  seed <- config$rsiena$estimation$seed
  if (is.null(seed)) seed <- config$rsiena$project_seed
  if (!is.null(seed)) seed <- as.integer(seed)

  # --- Log configuration -----------------------------------------------------
  message("[microstep] === SAOM Microstep Chain Extraction ===")
  message(sprintf("[microstep] Model: %s", model_id))
  message(sprintf("[microstep] Chains to extract (n3): %d", n_chains))
  message(sprintf("[microstep] Base fit: %s", fit_path_abs))
  message(sprintf("[microstep] Network arrays: %s", network_path_abs))
  message(sprintf("[microstep] Model spec: %s", spec_path_abs))
  message(sprintf("[microstep] Output: %s", output_path))
  if (!is.null(seed)) message(sprintf("[microstep] Seed: %d", seed))

  # --- Load base fit ---------------------------------------------------------
  message("[microstep] Loading base fit ...")
  base_fit <- load_base_fit(fit_path_abs)
  message("[microstep] Base fit loaded successfully.")

  # --- Build dataset ---------------------------------------------------------
  message("[microstep] Building SAOM dataset and effects ...")
  chain_data <- build_chain_dataset(
    network_path = network_path_abs,
    model_spec_path = spec_path_abs,
    model_id = model_id,
    repo_root = repo_root,
    config = config
  )
  message("[microstep] Dataset and effects configured.")

  # --- Extract chains --------------------------------------------------------
  message(sprintf("[microstep] Extracting %d chains via siena07 ...", n_chains))
  start_time <- Sys.time()

  chain_fit <- extract_chains(
    base_fit = base_fit,
    saom_data = chain_data$data,
    effects = chain_data$effects,
    algo_params = chain_data$algo_params,
    n_chains = n_chains,
    seed = seed
  )

  extraction_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  message(sprintf("[microstep] Extraction completed in %.1f seconds.", extraction_time))

  # --- Flatten chains --------------------------------------------------------
  message("[microstep] Flattening chain structure ...")
  n_periods <- length(WAVE_TRANSITIONS)
  chains_df <- flatten_chains(chain_fit, n_chains, n_periods)
  message(sprintf(
    "[microstep] Flattened %d microsteps across %d chains and %d periods.",
    nrow(chains_df), n_chains, n_periods
  ))

  # --- Build output payload --------------------------------------------------
  output <- list(
    chains = chains_df,
    metadata = list(
      model_id = model_id,
      n_chains = n_chains,
      seed = seed,
      n3_used = n_chains,
      extraction_time = extraction_time,
      wave_transitions = vapply(WAVE_TRANSITIONS, function(wt) wt$label, character(1))
    )
  )

  # --- Save ------------------------------------------------------------------
  saveRDS(output, output_path)
  message(sprintf("[microstep] Chains saved to %s", output_path))

  message("[microstep] === Chain extraction complete ===")

  invisible(output)
}

# --- Entry point (guarded) ---------------------------------------------------

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(err) {
      message(sprintf("[microstep] Error: %s", conditionMessage(err)))
      quit(status = 1)
    }
  )
}
