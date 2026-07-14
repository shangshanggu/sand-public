#!/usr/bin/env Rscript

SCRIPT_FILENAME <- "02_run_saom_model.R"

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- grep(file_arg, cmd_args, fixed = TRUE)
  if (length(matches) > 0) {
    script_path <- sub(file_arg, "", cmd_args[matches[1]])
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
  }

  frames <- sys.frames()
  if (length(frames) > 0) {
    for (idx in rev(seq_along(frames))) {
      env <- frames[[idx]]
      if (exists("ofile", envir = env, inherits = FALSE)) {
        candidate <- get("ofile", envir = env, inherits = FALSE)
        if (!is.null(candidate) && nzchar(candidate)) {
          return(dirname(normalizePath(candidate, winslash = "/", mustWork = TRUE)))
        }
      }
    }
  }

  default_candidates <- c(
    file.path("reproduced", "analyses", "chapter7_saom", "scripts", SCRIPT_FILENAME),
    file.path("analyses", "chapter7_saom", "scripts", SCRIPT_FILENAME)
  )
  for (candidate in default_candidates) {
    if (file.exists(candidate)) {
      return(dirname(normalizePath(candidate, winslash = "/", mustWork = TRUE)))
    }
  }

  normalizePath(".", winslash = "/", mustWork = TRUE)
}


script_dir <- get_script_dir()
repo_root_candidate <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root_candidate, "config", "thesis.yml"))) {
  search_roots <- unique(c(
    file.path(script_dir, "..", ".."),
    file.path(script_dir, ".."),
    file.path(getwd(), "reproduced"),
    "reproduced"
  ))
  found_root <- NULL
  for (candidate in search_roots) {
    candidate_norm <- tryCatch(
      normalizePath(candidate, winslash = "/", mustWork = FALSE),
      error = function(...) NA_character_
    )
    if (!is.na(candidate_norm) && file.exists(file.path(candidate_norm, "config", "thesis.yml"))) {
      found_root <- candidate_norm
      break
    }
  }
  if (!is.null(found_root)) {
    repo_root_candidate <- found_root
  }
}
repo_root <- normalizePath(repo_root_candidate, winslash = "/", mustWork = TRUE)
package_installer <- file.path(repo_root, "scripts", "00_setup", "install_r_packages.R")

ensure_runtime_packages <- function(packages, installer_path, repo_root) {
  packages <- unique(packages[!is.na(packages) & nzchar(packages)])
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) {
    return(invisible(TRUE))
  }

  if (!is.null(installer_path) && file.exists(installer_path)) {
    message(sprintf(
      "[chapter7] Installing missing packages via %s: %s",
      installer_path,
      paste(missing, collapse = ", ")
    ))
    status <- tryCatch({
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(repo_root)
      system2(
        "Rscript",
        c("--vanilla", installer_path, "--packages", paste(missing, collapse = ",")),
        stdout = "",
        stderr = "",
        wait = TRUE
      )
    }, error = function(err) {
      message(sprintf("[chapter7] Failed to invoke installer: %s", conditionMessage(err)))
      return(1L)
    })
    if (!identical(status, 0L)) {
      message(sprintf("[chapter7] Installer exited with status %s", status))
    }
  } else {
    installer_label <- if (!is.null(installer_path) && nzchar(installer_path)) installer_path else "<unset>"
    message(sprintf("[chapter7] Package installer not found at %s", installer_label))
  }

  missing_after <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_after)) {
    install_hint <- paste(sprintf("'%s'", missing_after), collapse = ", ")
    stop(
      sprintf(
        "Missing required R packages: %s. Install them via `make chapter7` or `R -e \"install.packages(c(%s))\"`.",
        paste(missing_after, collapse = ", "),
        install_hint
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

ensure_runtime_packages(c("yaml", "jsonlite"), package_installer, repo_root)

# Suppress tcltk to prevent 'tkvars' errors in headless environments.
# RSiena 1.4.7 registers an on.exit handler (exitfn) that references tkvars
# before is.batch() is set to TRUE.  If siena07 errors early, the handler
# fires with is.batch()==FALSE and crashes on the missing tkvars object.
# Fix: pre-set is.batch to TRUE so the handler never touches tcltk.
if (!nzchar(Sys.getenv("DISPLAY", ""))) {
  Sys.setenv(DISPLAY = "")
  options(device = "pdf")
}
tryCatch({
  rsiena_ns <- asNamespace("RSiena")
  is_batch_fn <- get("is.batch", envir = rsiena_ns)
  is_batch_fn(TRUE)
}, error = function(e) {
  # RSiena not yet loaded; will be set in run_saom() before siena07 call
  NULL
})

suppressPackageStartupMessages({
  library(jsonlite)
  library(yaml)
  library(parallel)
})

is_placeholder_fit <- function(obj) {
  inherits(obj, "saomPlaceholder") || isTRUE(obj$placeholder)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (is.character(x) && length(x) == 1 && !nzchar(x)) {
    return(y)
  }
  x
}

merge_lists <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (is.null(y) || length(y) == 0) {
    return(x)
  }
  modifyList(x, y)
}

filter_algorithm_params <- function(params) {
  if (is.null(params) || length(params) == 0) {
    return(list())
  }
  remapped <- params
  if (!is.null(remapped$n2) && is.null(remapped$n2start)) {
    remapped$n2start <- remapped$n2
  }
  remapped$n2 <- NULL
  remapped$max_iterations <- NULL

  valid <- c(
    "n3",
    "nsub",
    "n2start",
    "firstg",
    "reduceg",
    "maxlike",
    "simOnly",
    "diagonalize",
    "dolby",
    "seed",
    "maximumPermutationLength",
    "minimumPermutationLength",
    "initialPermutationLength"
  )
  if (requireNamespace("RSiena", quietly = TRUE)) {
    valid <- intersect(valid, names(formals(RSiena::sienaAlgorithmCreate)))
  }
  filtered <- remapped[names(remapped) %in% valid]
  integer_fields <- c("n3", "nsub", "n2start", "seed", "maximumPermutationLength", "minimumPermutationLength", "initialPermutationLength")
  for (field in integer_fields) {
    if (!is.null(filtered[[field]]) && length(filtered[[field]]) > 0) {
      filtered[[field]] <- suppressWarnings(as.integer(filtered[[field]]))
    }
  }
  filtered
}

map_network_effect <- function(name) {
  mapping <- c(
    outdegree = "density",
    reciprocity = "recip",
    transitiveTriplets = "transTrip",
    threeCycle = "cycle3",
    outdegreeActivity = "outAct",
    indegreeActivity = "inAct"
  )
  value <- mapping[name]
  if (length(value) == 0 || is.na(value)) {
    name
  } else {
    unname(value)
  }
}

map_behaviour_effect <- function(name) {
  mapping <- c(
    linearShape = "linear",
    quadShape = "quad"
  )
  value <- mapping[name]
  if (length(value) == 0 || is.na(value)) {
    name
  } else {
    unname(value)
  }
}

get_rng_settings <- function() {
  kinds <- RNGkind()
  list(
    kind = kinds[1],
    normal_kind = if (length(kinds) > 1) kinds[2] else NULL,
    sample_kind = if (length(kinds) > 2) kinds[3] else NULL
  )
}

write_csv_safely <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_csv(data, path)
  } else {
    utils::write.csv(data, path, row.names = FALSE)
  }
  path
}

sanitize_identifier <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return("default")
  }
  sanitized <- gsub("[^A-Za-z0-9]+", "_", text)
  sanitized <- gsub("_+", "_", sanitized)
  sanitized <- sub("^_+", "", sanitized)
  sanitized <- sub("_+$", "", sanitized)
  if (!nzchar(sanitized)) {
    return("default")
  }
  sanitized
}

build_checkpoint_filename <- function(model_id, phase, batch_index, timestamp = Sys.time(), tz = "UTC") {
  phase_component <- if (is.null(phase) || is.na(phase)) "unknown" else sprintf("phase%02d", as.integer(phase))
  batch_component <- if (is.null(batch_index) || is.na(batch_index)) "batch000" else sprintf("batch%03d", as.integer(batch_index))
  time_component <- format(as.POSIXct(timestamp, tz = tz), "%Y%m%dT%H%M%SZ", tz = tz)
  sprintf("saom_%s_%s_%s_%s.rds", sanitize_identifier(model_id), phase_component, batch_component, time_component)
}

save_saom_checkpoint <- function(dir, model_id, fit, metadata, filename = NULL, tz = "UTC") {
  if (is.null(dir) || !nzchar(dir)) {
    stop("Checkpoint directory must be provided.")
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file_name <- filename %||% build_checkpoint_filename(model_id, metadata$phase %||% NA_integer_, metadata$batch_index %||% NA_integer_, metadata$created_at %||% Sys.time(), tz = tz)
  path <- file.path(dir, file_name)
  payload <- list(fit = fit, metadata = metadata)
  tryCatch(
    saveRDS(payload, path),
    error = function(err) {
      stop(sprintf("Failed to write SAOM checkpoint at %s: %s", path, conditionMessage(err)), call. = FALSE)
    }
  )
  path
}

list_saom_checkpoints <- function(dir, model_id) {
  if (is.null(dir) || !dir.exists(dir)) {
    return(character(0))
  }
  pattern <- sprintf("^saom_%s_.*\\.rds$", sanitize_identifier(model_id))
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) {
    return(character(0))
  }
  files[order(file.mtime(files), decreasing = TRUE)]
}

load_saom_checkpoint_state <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    {
      payload <- readRDS(path)
      if (!is.list(payload) || is.null(payload$fit)) {
        stop(sprintf("Checkpoint %s is malformed.", path))
      }
      payload$path <- path
      payload
    },
    error = function(err) {
      message(sprintf("[rsiena] Unable to load checkpoint %s: %s", path, conditionMessage(err)))
      NULL
    }
  )
}

load_latest_checkpoint <- function(dir, model_id) {
  candidates <- list_saom_checkpoints(dir, model_id)
  if (!length(candidates)) {
    return(NULL)
  }
  for (candidate in candidates) {
    payload <- load_saom_checkpoint_state(candidate)
    if (!is.null(payload)) {
      return(payload)
    }
  }
  NULL
}

extract_convergence_stat <- function(fit) {
  if (is.null(fit)) {
    return(NA_real_)
  }
  if (!is.null(fit$tconv.max)) {
    return(as.numeric(fit$tconv.max))
  }
  if (!is.null(fit$tconv)) {
    if (is.matrix(fit$tconv)) {
      return(as.numeric(max(abs(fit$tconv[nrow(fit$tconv), ]))))
    }
    if (is.numeric(fit$tconv)) {
      return(as.numeric(max(abs(fit$tconv))))
    }
  }
  NA_real_
}


resolve_repo_path <- function(repo_root, path, must_work = TRUE) {
  if (is.null(path) || length(path) == 0L || !nzchar(path[1])) {
    stop("Path must be a non-empty string.")
  }

  candidate <- path[1]

  if (grepl("^/", candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = must_work))
  }

  candidate <- gsub("\\", "/", candidate, fixed = TRUE)
  candidate <- sub("^\\./+", "", candidate)
  candidate <- sub("^reproduced/+", "", candidate)

  full_path <- file.path(repo_root, candidate)
  normalizePath(full_path, winslash = "/", mustWork = must_work)
}

normalize_theta_matrix <- function(theta_obj) {
  if (is.null(theta_obj)) {
    return(NULL)
  }

  if (is.matrix(theta_obj)) {
    return(theta_obj)
  }

  if (is.data.frame(theta_obj)) {
    return(as.matrix(theta_obj))
  }

  if (is.atomic(theta_obj) && length(theta_obj) > 0) {
    mat <- matrix(as.numeric(theta_obj), ncol = 1)
    rownames(mat) <- names(theta_obj)
    colnames(mat) <- if (is.null(colnames(mat))) "estimate" else colnames(mat)
    return(mat)
  }

  NULL
}

call_install_script <- function(script_path) {
  if (is.null(script_path) || !nzchar(script_path) || !file.exists(script_path)) {
    return(FALSE)
  }

  message("[rsiena] Attempting automatic installation via ", script_path)
  result <- try(system2("Rscript", c("--vanilla", script_path), stdout = TRUE, stderr = TRUE), silent = TRUE)
  if (inherits(result, "try-error")) {
    message("[rsiena] Installation script raised an error: ", conditionMessage(attr(result, "condition")))
    return(FALSE)
  }

  status_code <- attr(result, "status")
  if (!is.null(status_code) && !identical(status_code, 0L)) {
    message(sprintf("[rsiena] Installation script exited with status %s", status_code))
    return(FALSE)
  }

  requireNamespace("RSiena", quietly = TRUE)
}

truthy_strings <- c("1", "true", "t", "yes", "y", "on")
falsy_strings <- c("0", "false", "f", "no", "n", "off")

resolve_auto_install_preference <- function(config, override) {
  if (!is.null(override)) {
    return(isTRUE(override))
  }

  env_value <- Sys.getenv("RSIENA_AUTO_INSTALL", unset = NA_character_)
  if (!is.na(env_value) && nzchar(env_value)) {
    env_lower <- tolower(env_value)
    if (env_lower %in% truthy_strings) {
      return(TRUE)
    }
    if (env_lower %in% falsy_strings) {
      return(FALSE)
    }
  }

  config_flag <- config$rsiena$installation$auto_install
  if (!is.null(config_flag)) {
    return(isTRUE(config_flag))
  }

  FALSE
}

extract_siena_fit <- function(env) {
  for (name in ls(env, all.names = TRUE)) {
    obj <- get(name, envir = env, inherits = FALSE)
    if (inherits(obj, "sienaFit")) {
      return(list(name = name, fit = obj))
    }
  }
  NULL
}

load_cached_fit <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }

  env <- new.env(parent = emptyenv())
  loaded <- try(load(path, envir = env), silent = TRUE)
  if (inherits(loaded, "try-error")) {
    message("[rsiena] Unable to load cached RSiena fit from ", path, ": ", conditionMessage(attr(loaded, "condition")))
    return(NULL)
  }

  fit_entry <- extract_siena_fit(env)
  if (is.null(fit_entry)) {
    message("[rsiena] Cached file ", path, " does not contain a 'sienaFit' object.")
    return(NULL)
  }

  list(fit = fit_entry$fit, name = fit_entry$name)
}

load_network_payload <- function(path) {
  payload <- readRDS(path)
  if (is.null(payload$network_array) || is.null(payload$behaviour_array)) {
    stop("network_arrays.rds is missing required components.")
  }
  payload
}

prepare_actor_covariates <- function(payload, ids) {
  actor_covs <- payload$actor_covariates
  if (is.null(actor_covs) || nrow(actor_covs) == 0) {
    return(list(covars = list(), names = character(0)))
  }

  actor_covs$redcap_survey_identifier <- as.character(actor_covs$redcap_survey_identifier)
  actor_covs <- actor_covs[match(as.character(ids), actor_covs$redcap_survey_identifier), , drop = FALSE]
  actor_covs <- actor_covs[, setdiff(names(actor_covs), c("friend_number")), drop = FALSE]

  covar_objects <- list()
  covar_names <- character(0)

  for (col in setdiff(names(actor_covs), "redcap_survey_identifier")) {
    values <- actor_covs[[col]]
    if (all(is.na(values))) {
      next
    }
    covar_objects[[col]] <- RSiena::coCovar(as.numeric(values))
    covar_names <- c(covar_names, col)
  }

  list(covars = covar_objects, names = covar_names)
}

prepare_dyadic_covariates <- function(payload, ids) {
  dyadic_covs <- payload$dyadic_covariates
  if (is.null(dyadic_covs) || length(dyadic_covs) == 0) {
    return(list(covars = list(), names = character(0)))
  }

  covar_objects <- list()
  covar_names <- character(0)

  for (name in names(dyadic_covs)) {
    matrix_value <- dyadic_covs[[name]]
    if (is.null(matrix_value)) {
      next
    }
    dimnames(matrix_value) <- list(as.character(ids), as.character(ids))
    covar_objects[[name]] <- RSiena::coDyadCovar(matrix_value)
    covar_names <- c(covar_names, name)
  }

  list(covars = covar_objects, names = covar_names)
}

build_saom_dataset <- function(payload, model_spec) {
  ids <- payload$ids
  waves <- payload$waves %||% paste0("wave", seq_len(dim(payload$network_array)[3]))

  network_array <- payload$network_array

  behaviour_array <- payload$behaviour_array

  network_name <- model_spec$network_variable %||% "friendship"
  behaviour_name <- model_spec$behavior_variable %||% "behaviour"

  network_dep <- RSiena::sienaDependent(network_array, type = "oneMode", nodeSet = "Actors", allowOnly = FALSE)
  behaviour_dep <- RSiena::sienaDependent(behaviour_array, type = "behavior", nodeSet = "Actors")

  dataset_args <- list()
  dataset_args[[network_name]] <- network_dep
  dataset_args[[behaviour_name]] <- behaviour_dep

  actor_covs <- prepare_actor_covariates(payload, ids)
  for (name in names(actor_covs$covars)) {
    dataset_args[[name]] <- actor_covs$covars[[name]]
  }

  if (!is.null(payload$behaviour_lag)) {
    lag_matrix <- payload$behaviour_lag
    dataset_args[["audit_score_previous"]] <- RSiena::varCovar(lag_matrix)
    actor_covs$names <- unique(c(actor_covs$names, "audit_score_previous"))
  }

  dyadic_covs <- prepare_dyadic_covariates(payload, ids)
  for (name in names(dyadic_covs$covars)) {
    dataset_args[[name]] <- dyadic_covs$covars[[name]]
  }

  saom_data <- do.call(RSiena::sienaDataCreate, dataset_args)

  list(
    data = saom_data,
    network_name = network_name,
    behaviour_name = behaviour_name,
    actor_covariates = actor_covs$names,
    dyadic_covariates = dyadic_covs$names
  )
}

configure_effects <- function(saom_data, model_spec, network_name, behaviour_name, actor_covs, dyadic_covs) {
  if (is.null(saom_data)) {
    return(list(effects = NULL, skipped = list(network = character(0), behaviour = character(0))))
  }

  effects <- RSiena::getEffects(saom_data)
  skipped <- list(network = character(0), behaviour = character(0))

  if (!is.null(model_spec$network_effects)) {
    for (effect_cfg in model_spec$network_effects) {
      if (identical(effect_cfg$name, "outdegree")) {
        skipped$network <- c(skipped$network, "outdegree (default density effect already included)")
        next
      }

      effect_name <- map_network_effect(effect_cfg$name)
      attr <- effect_cfg$attribute
      param <- effect_cfg$parameter

      if (!is.null(attr)) {
        valid_interactions <- unique(c(actor_covs, dyadic_covs, behaviour_name))
        if (!(attr %in% valid_interactions)) {
          skipped$network <- c(skipped$network, sprintf("%s (missing covariate %s)", effect_cfg$name, attr))
          next
        }
      }

      args <- list(effects, effect_name, name = network_name, character = TRUE)
      if (!is.null(attr)) {
        args$interaction1 <- attr
      }
      if (!is.null(param)) {
        args$parameter <- param
      }

      try_result <- try(do.call(RSiena::includeEffects, args), silent = TRUE)
      if (inherits(try_result, "try-error")) {
        skipped$network <- c(skipped$network, sprintf("%s (include failed: %s)", effect_cfg$name, conditionMessage(attr(try_result, "condition"))))
      } else {
        effects <- try_result
      }
    }
  }

  if (!is.null(model_spec$behavior_effects)) {
    for (effect_cfg in model_spec$behavior_effects) {
      effect_name <- map_behaviour_effect(effect_cfg$name)
      attr <- effect_cfg$attribute
      param <- effect_cfg$parameter
      variant <- effect_cfg$variant

      if (identical(effect_cfg$name, "linear") && !is.null(attr)) {
        effect_name <- "effFrom"
      }

      args <- list(effects, effect_name, name = behaviour_name, character = TRUE)

      if (effect_name %in% c("avAlt", "avSim", "indeg", "outdeg")) {
        args$interaction1 <- network_name
      }

      if (!is.null(attr)) {
        if (attr %in% actor_covs) {
          args$interaction1 <- attr
        } else if (attr %in% dyadic_covs) {
          args$interaction1 <- attr
        } else {
          skipped$behaviour <- c(skipped$behaviour, sprintf("%s (missing covariate %s)", effect_cfg$name, attr))
          next
        }
      }

      if (!is.null(variant)) {
        args$interaction1 <- variant
      }

      if (!is.null(param)) {
        args$parameter <- param
      }

      try_result <- try(do.call(RSiena::includeEffects, args), silent = TRUE)
      if (inherits(try_result, "try-error")) {
        skipped$behaviour <- c(skipped$behaviour, sprintf("%s (include failed: %s)", effect_cfg$name, conditionMessage(attr(try_result, "condition"))))
      } else {
        effects <- try_result
      }
    }
  }

  list(effects = effects, skipped = skipped)
}

run_saom <- function(algorithm_params, saom_data, effects, project_seed, projname, theta_bound = 100, prev_ans = NULL, n_cores = NULL) {
  if (!is.null(project_seed)) {
    if (exists("set_deterministic_seed", mode = "function")) {
      set_deterministic_seed(project_seed)
    } else {
      set.seed(project_seed)
    }
  }

  algo_args <- merge_lists(list(projname = projname), algorithm_params)
  algorithm <- do.call(RSiena::sienaAlgorithmCreate, algo_args)
  algorithm$thetaBound <- c(-abs(theta_bound), abs(theta_bound))

  siena_args <- list(
    algorithm,
    data = saom_data,
    effects = effects,
    batch = TRUE,
    returnDeps = TRUE,
    silent = TRUE,
    prevAns = prev_ans
  )

  # Force batch mode in RSiena's internal state before calling siena07.
  # This prevents the on.exit handler from trying to access tkvars if
  # siena07 errors before it reaches its own is.batch(batchUse) call.
  tryCatch({
    rsiena_ns <- asNamespace("RSiena")
    is_batch_fn <- get("is.batch", envir = rsiena_ns)
    is_batch_fn(TRUE)
  }, error = function(e) NULL)

  if ("thetaBound" %in% names(formals(RSiena::siena07))) {
    siena_args$thetaBound <- theta_bound
  }

  # Parallel estimation: create a PSOCK cluster and export RSiena lib paths
  cl <- NULL
  if (!is.null(n_cores) && n_cores > 1L) {
    cl <- tryCatch({
      message(sprintf("[rsiena] Creating PSOCK cluster with %d workers...", n_cores))
      cluster <- parallel::makeCluster(n_cores)
      rsiena_lib_paths <- .libPaths()
      parallel::clusterCall(cluster, function(paths) .libPaths(paths), rsiena_lib_paths)
      message("[rsiena] Cluster ready")
      cluster
    }, error = function(e) {
      message(sprintf("[rsiena] Cluster creation failed: %s; falling back to single-threaded", e$message))
      NULL
    })
  }

  if (!is.null(cl)) {
    on.exit(tryCatch(parallel::stopCluster(cl), error = function(e) {
      message(sprintf("[rsiena] Cluster cleanup warning (non-fatal): %s", e$message))
    }), add = TRUE)
    siena_args$useCluster <- TRUE
    siena_args$nbrNodes <- n_cores
    siena_args$cl <- cl
  }

  do.call(RSiena::siena07, siena_args)
}

parse_args <- function(default_model) {
  args <- commandArgs(trailingOnly = TRUE)
  model <- default_model
  auto_install <- NULL
  if (length(args) == 0) {
    return(list(model = model, auto_install = auto_install))
  }

  i <- 1L
  positional_consumed <- FALSE
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--model", "-m")) {
      if (i == length(args)) {
        stop("--model flag provided without a value.")
      }
      model <- args[[i + 1L]]
      positional_consumed <- TRUE
      i <- i + 2L
      next
    }

    if (grepl("^--model=", arg)) {
      model <- sub("^--model=", "", arg)
      positional_consumed <- TRUE
      i <- i + 1L
      next
    }

    if (arg == "--auto-install") {
      auto_install <- TRUE
      i <- i + 1L
      next
    }

    if (arg == "--no-auto-install") {
      auto_install <- FALSE
      i <- i + 1L
      next
    }

    if (!startsWith(arg, "-") && !positional_consumed) {
      model <- arg
      positional_consumed <- TRUE
      i <- i + 1L
      next
    }

    stop(sprintf("Unrecognised argument '%s'", arg))
  }

  list(model = model, auto_install = auto_install)
}

main <- function() {
  repo_root_local <- repo_root
  setwd(repo_root_local)
  source(file.path(repo_root_local, "R", "common.R"))

  config <- yaml::read_yaml(file.path("config", "thesis.yml"))
  chapter_cfg <- config$chapters$chapter7_saom
  if (is.null(chapter_cfg)) {
    stop("chapter7_saom configuration block is missing from reproduced/config/thesis.yml")
  }

  spec_relative <- chapter_cfg$model_specs_file %||% file.path("config", "scenarios", "saom_models.yml")
  spec_path <- tryCatch(
    resolve_repo_path(repo_root_local, spec_relative, must_work = TRUE),
    error = function(e) {
      stop(sprintf(
        "SAOM model specifications not found at %s (resolved via %s): %s",
        spec_relative,
        repo_root_local,
        conditionMessage(e)
      ))
    }
  )

  if (!file.exists(spec_path)) {
    stop(sprintf("SAOM model specifications not found at %s", spec_path))
  }

  spec <- yaml::read_yaml(spec_path)
  if (is.null(spec$models) || length(spec$models) == 0) {
    stop("No SAOM models defined in the specification file.")
  }

  parsed_args <- parse_args(chapter_cfg$default_model %||% names(spec$models)[1])
  model_id <- parsed_args$model
  auto_install_override <- parsed_args$auto_install
  if (!model_id %in% names(spec$models)) {
    stop(sprintf("Model '%s' not found in %s", model_id, spec_path))
  }
  model_spec <- spec$models[[model_id]]

  timezone <- config$project$timezone %||% "UTC"

  outputs_relative <- chapter_cfg$outputs_dir %||% "outputs/chapter7"
  outputs_dir <- tryCatch(
    resolve_repo_path(repo_root_local, outputs_relative, must_work = FALSE),
    error = function(err) {
      message(sprintf(
        "[rsiena] Unable to resolve outputs directory via %s: %s",
        outputs_relative,
        conditionMessage(err)
      ))
      file.path(repo_root_local, outputs_relative)
    }
  )
  dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)

  logs_relative <- config$rsiena$diagnostics$diagnostics_dir %||% file.path(outputs_relative, "logs")
  logs_dir <- tryCatch(
    resolve_repo_path(repo_root_local, logs_relative, must_work = FALSE),
    error = function(err) {
      message(sprintf(
        "[rsiena] Unable to resolve diagnostics directory via %s: %s",
        logs_relative,
        conditionMessage(err)
      ))
      file.path(repo_root_local, logs_relative)
    }
  )
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

  cache_relative_default <- file.path(outputs_relative, "cache")
  cache_relative <- chapter_cfg$cache_dir %||% cache_relative_default
  cache_dir <- tryCatch(
    resolve_repo_path(repo_root_local, cache_relative, must_work = FALSE),
    error = function(err) {
      message(sprintf(
        "[rsiena] Unable to resolve cache directory via %s: %s",
        cache_relative,
        conditionMessage(err)
      ))
      file.path(repo_root_local, cache_relative)
    }
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  fit_path <- file.path(cache_dir, sprintf("%s_fit.RData", model_id))
  estimation_mode <- "estimation"
  result <- NULL
  cached_source <- NULL
  run_timing <- NULL
  # Placeholders are permanently disabled. The pipeline must either run a real

  # RSiena estimation or fail hard — no silent fake results.
  placeholder_enabled <- FALSE


  network_relative <- NULL
  for (path in chapter_cfg$required_inputs %||% character()) {
    if (grepl("network_arrays\\.rds$", path)) {
      network_relative <- path
      break
    }
  }
  if (is.null(network_relative)) {
    stop("chapter7_saom.required_inputs must include the network_arrays.rds path.")
  }
  network_path <- resolve_repo_path(repo_root_local, network_relative, must_work = FALSE)
  data_warnings <- list()
  if (!file.exists(network_path)) {
    stop(
      sprintf(
        "Required network arrays not found at %s. Complete the Chapter 4/5 data cleaning pipeline or run 00_build_network_arrays_base.R before Chapter 7.",
        network_relative
      )
    )
  }

  payload <- load_network_payload(network_path)
  payload_metadata <- payload$metadata %||% list()
  actor_covariate_names <- if (!is.null(payload$actor_covariates)) {
    setdiff(names(payload$actor_covariates), c("redcap_survey_identifier"))
  } else {
    character(0)
  }
  dyadic_covariate_names <- if (!is.null(payload$dyadic_covariates)) names(payload$dyadic_covariates) else character(0)
  dyadic_nonzero_counts <- if (!is.null(payload$dyadic_covariates)) {
    vapply(payload$dyadic_covariates, function(value) sum(value != 0, na.rm = TRUE), integer(1))
  } else {
    integer(0)
  }
  synthetic_source <- payload_metadata$synthetic_source %||% payload$synthetic_source
  placeholder_inputs <- isTRUE(payload$placeholder) || isTRUE(payload_metadata$placeholder) || identical(synthetic_source, "chapter7_placeholder")

  if (placeholder_inputs) {
    data_warnings <- c(data_warnings, "network_arrays generated from synthetic placeholder inputs for Chapter 7 smoke tests")
  } else if (!is.null(synthetic_source) && nzchar(synthetic_source)) {
    data_warnings <- c(data_warnings, sprintf("network_arrays derived from source '%s'", synthetic_source))
  }

  network_tie_count <- sum(payload$network_array, na.rm = TRUE)
  total_possible_ties <- prod(dim(payload$network_array)[1:2]) * dim(payload$network_array)[3]

  if (is.na(network_tie_count) || network_tie_count <= 0) {
    data_warnings <- c(data_warnings, "network_array contains no observed ties")
  }

  dataset_info <- list(
    data = NULL,
    network_name = model_spec$network_variable %||% "friendship",
    behaviour_name = model_spec$behavior_variable %||% "behaviour",
    actor_covariates = actor_covariate_names,
    dyadic_covariates = dyadic_covariate_names,
    dyadic_nonzero_counts = dyadic_nonzero_counts
  )

  effects_info <- list(effects = NULL, skipped = list(network = character(0), behaviour = character(0)))



  resume_cfg <- config$rsiena$resume %||% list()
  resume_enabled <- isTRUE(resume_cfg$enabled)
  checkpoint_relative_default <- file.path(chapter_cfg$outputs_dir %||% "outputs/chapter7", "checkpoints")
  checkpoint_relative <- resume_cfg$checkpoint_dir %||% checkpoint_relative_default
  checkpoint_dir <- tryCatch(
    resolve_repo_path(repo_root_local, checkpoint_relative, must_work = FALSE),
    error = function(err) {
      message(sprintf("[rsiena] Unable to resolve checkpoint directory via %s: %s", checkpoint_relative, conditionMessage(err)))
      file.path(repo_root_local, checkpoint_relative)
    }
  )
  resume_state <- NULL
  resume_prev_ans <- NULL
  resume_batches_completed <- 0L
  resume_completed_checkpoint <- FALSE
  last_checkpoint_path <- NULL
  checkpoints_written <- 0L
  resumed_from_path <- NULL
  if (resume_enabled) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    resume_state <- load_latest_checkpoint(checkpoint_dir, model_id)
    if (!is.null(resume_state)) {
      resume_prev_ans <- resume_state$fit
      resume_batches_completed <- resume_state$metadata$batch_index %||% 0L
      last_checkpoint_path <- resume_state$path
      resumed_from_path <- resume_state$path
      if (isTRUE(resume_state$metadata$completed)) {
        result <- resume_prev_ans
        estimation_mode <- "checkpoint"
        cached_source <- resume_state$path
        resume_completed_checkpoint <- TRUE
      } else {
        message(sprintf(
          "Resuming model '%s' from checkpoint %s (phase %s)",
          model_id,
          basename(resume_state$path),
          resume_state$metadata$phase %||% "<unknown>"
        ))
      }
    }
  } else {
    checkpoint_dir <- NULL
  }

  install_script_relative <- config$rsiena$installation$script_path %||% file.path("scripts", "00_setup", "install_rsiena.R")
  install_script_path <- resolve_repo_path(repo_root_local, install_script_relative, must_work = FALSE)

  auto_install_enabled <- resolve_auto_install_preference(config, auto_install_override)
  auto_install_attempted <- FALSE
  auto_install_succeeded <- FALSE

  rsiena_install_cfg <- config$rsiena$installation %||% list()
  library_dir_relative <- rsiena_install_cfg$library_dir %||% NULL
  if (!is.null(library_dir_relative) && nzchar(library_dir_relative)) {
    library_dir <- tryCatch(
      resolve_repo_path(repo_root_local, library_dir_relative, must_work = FALSE),
      error = function(err) NULL
    )
    if (!is.null(library_dir) && dir.exists(library_dir)) {
      .libPaths(c(library_dir, .libPaths()))
    }
  }

  rsiena_available <- requireNamespace("RSiena", quietly = TRUE)
  if (!rsiena_available) {
    if (auto_install_enabled) {
      auto_install_attempted <- TRUE
      auto_install_succeeded <- isTRUE(call_install_script(install_script_path))
    } else {
      message(
        "[rsiena] RSiena package not installed and automatic installation is disabled.",
        " Set RSIENA_AUTO_INSTALL=1, pass --auto-install, or enable rsiena.installation.auto_install in the config to allow automatic downloads."
      )
    }
    rsiena_available <- requireNamespace("RSiena", quietly = TRUE)
  }

  if (!rsiena_available) {
    install_hint <- sprintf(
      "Install RSiena via 'make rsiena', run %s manually, or enable automatic installation via --auto-install or RSIENA_AUTO_INSTALL=1.",
      install_script_relative
    )
    stop(
      sprintf("RSiena package is not installed. Cannot proceed with SAOM estimation.\n%s", install_hint),
      call. = FALSE
    )
  }

  # RSiena availability already confirmed above; this is a defensive check.
  if (!requireNamespace("RSiena", quietly = TRUE)) {
    stop("RSiena package disappeared between checks. Cannot proceed.", call. = FALSE)
  }

  dataset_info <- build_saom_dataset(payload, model_spec)

  effects_info <- configure_effects(
    dataset_info$data,
    model_spec,
    dataset_info$network_name,
    dataset_info$behaviour_name,
    dataset_info$actor_covariates,
    dataset_info$dyadic_covariates
  )

  algorithm_params <- spec$defaults$algorithm %||% list()
  algorithm_params <- merge_lists(algorithm_params, config$rsiena$estimation %||% list())
  algorithm_params <- merge_lists(algorithm_params, model_spec$algorithm %||% list())
  algorithm_params <- filter_algorithm_params(algorithm_params)
  if (is.null(algorithm_params$seed) && !is.null(config$rsiena$project_seed)) {
    algorithm_params$seed <- as.integer(config$rsiena$project_seed)
  }

  projname <- sprintf("saom_%s", model_id)
  cache_strategy <- config$rsiena$cache_strategy %||% "reuse"
  use_cached <- isTRUE(config$rsiena$use_cached_results) && !identical(cache_strategy, "refresh")
  cached_relative <- model_spec$cached_results
  if (is.null(cached_relative)) {
    cached_relative <- chapter_cfg$baseline_model$path
  }
  cached_absolute <- NULL
  if (!is.null(cached_relative)) {
    cached_absolute <- resolve_repo_path(repo_root, cached_relative, must_work = FALSE)
  }

  if (resume_completed_checkpoint && !is.null(result) && !is.null(cached_source)) {
    message(sprintf("Loaded converged checkpoint for model '%s' from %s", model_id, basename(cached_source)))
  }

  run_rsiena_with_context <- function(current_algorithm_params, prev_ans = NULL, batch_index = NULL) {
    if (!is.null(config$rsiena$project_seed)) {
      if (exists("set_deterministic_seed", mode = "function")) {
        set_deterministic_seed(config$rsiena$project_seed)
      } else {
        set.seed(config$rsiena$project_seed)
      }
    }

    context <- create_run_context(tz = timezone)
    if (!is.null(batch_index)) {
      context$metadata$batch_index <- batch_index
    }
    # Resolve parallelism: use config or detect available cores
    par_cfg <- config$rsiena$parallelization %||% list()
    n_cores <- if (isTRUE(par_cfg$enabled)) {
      par_cfg$cores %||% max(1L, parallel::detectCores() - 2L)
    } else {
      1L
    }

    fit <- try(
      run_saom(
        current_algorithm_params,
        dataset_info$data,
        effects_info$effects,
        config$rsiena$project_seed,
        projname,
        theta_bound = 500,
        prev_ans = prev_ans,
        n_cores = n_cores
      ),
      silent = TRUE
    )
    timing <- tryCatch(
      complete_run_context(context),
      error = function(err) {
        message(sprintf("Failed to finalise run context: %s", conditionMessage(err)))
        NULL
      }
    )
    list(result = fit, timing = timing)
  }

  if (is.null(result) && use_cached && !is.null(cached_absolute) && file.exists(cached_absolute)) {
    cached_obj <- load_cached_fit(cached_absolute)
    if (!is.null(cached_obj)) {
      cached_fit <- cached_obj$fit
      cached_source <- cached_absolute
      if (is_placeholder_fit(cached_fit)) {
        estimation_mode <- "cached_placeholder_ignored"
        message(sprintf(
          "Found placeholder RSiena fit at %s; ignoring cache and re-estimating model '%s'.",
          cached_relative,
          model_id
        ))
      } else {
        result <- cached_fit
        estimation_mode <- "cached"
        if (!file.exists(fit_path) || identical(cache_strategy, "refresh")) {
          save(result, dataset_info, effects_info, file = fit_path)
        }
        message(sprintf("Loaded cached RSiena fit from %s", cached_relative))
      }
    }
  }

  if (is.null(result)) {
    algorithm_attempt_params <- algorithm_params
    tolerance_limit <- resume_cfg$convergence_tolerance %||% config$rsiena$diagnostics$convergence_tolerance %||% chapter_cfg$convergence_tolerance %||% NA_real_
    phase_limit <- resume_cfg$target_phase
    if (is.null(phase_limit)) {
      phase_limit <- NA_real_
    }
    current_fit <- resume_prev_ans
    current_batch_index <- resume_batches_completed
    sim_retry_performed <- FALSE
    if (resume_enabled && !is.null(current_fit)) {
      estimation_mode <- "checkpoint_resume"
    }

    repeat {
      next_batch_index <- current_batch_index + 1L
      previous_phase <- if (!is.null(current_fit) && !is.null(current_fit$phase)) current_fit$phase else NA_real_

      run_attempt <- run_rsiena_with_context(algorithm_attempt_params, prev_ans = current_fit, batch_index = next_batch_index)
      candidate <- run_attempt$result
      run_timing <- run_attempt$timing

      if (inherits(candidate, "try-error")) {
        if (!sim_retry_performed) {
          message("Initial SAOM estimation failed: ", conditionMessage(attr(candidate, "condition")))
          algorithm_attempt_params$simOnly <- TRUE
          estimation_mode <- "simulation_only"
          sim_retry_performed <- TRUE
          next
        }

        result <- candidate
        break
      }

      result <- candidate
      current_fit <- candidate
      current_batch_index <- next_batch_index
      resume_batches_completed <- current_batch_index

      if (resume_enabled) {
        checkpoint_metadata <- list(
          model = model_id,
          batch_index = current_batch_index,
          phase = candidate$phase %||% NA_real_,
          created_at = Sys.time(),
          timezone = timezone,
          convergence = extract_convergence_stat(candidate),
          completed = FALSE,
          resumed_from = resumed_from_path,
          checkpoint_dir = checkpoint_dir,
          rng = get_rng_settings()
        )
        theta_snapshot <- tryCatch(normalize_theta_matrix(candidate$theta), error = function(err) NULL)
        if (!is.null(theta_snapshot) && nrow(theta_snapshot) > 0 && ncol(theta_snapshot) >= 1) {
          checkpoint_metadata$theta_estimate <- as.numeric(theta_snapshot[, 1, drop = TRUE])
          names(checkpoint_metadata$theta_estimate) <- rownames(theta_snapshot)
        }
        last_checkpoint_path <- save_saom_checkpoint(
          checkpoint_dir,
          model_id,
          candidate,
          checkpoint_metadata,
          tz = timezone
        )
        checkpoints_written <- checkpoints_written + 1L
      }

      conv_stat <- extract_convergence_stat(candidate)
      phase_val <- candidate$phase %||% NA_real_
      termination_text <- if (!is.null(candidate$termination)) tolower(paste(candidate$termination, collapse = " ")) else ""

      should_stop <- FALSE
      if (!is.na(conv_stat) && !is.na(tolerance_limit) && conv_stat <= tolerance_limit) {
        should_stop <- TRUE
      }
      if (!should_stop && !is.na(phase_limit) && !is.na(phase_val) && phase_val >= phase_limit) {
        should_stop <- TRUE
      }
      if (!should_stop && nzchar(termination_text)) {
        if (grepl("converg", termination_text) || grepl("max", termination_text) || grepl("stop", termination_text)) {
          should_stop <- TRUE
        }
      }
      if (!should_stop && resume_enabled && !is.na(previous_phase) && !is.na(phase_val) && phase_val <= previous_phase) {
        should_stop <- TRUE
      }
      if (!should_stop && !resume_enabled) {
        should_stop <- TRUE
      }

      if (should_stop) {
        if (resume_enabled && !is.null(last_checkpoint_path)) {
          checkpoint_payload <- load_saom_checkpoint_state(last_checkpoint_path)
          if (!is.null(checkpoint_payload)) {
            checkpoint_payload$metadata$completed <- TRUE
            checkpoint_payload$metadata$created_at <- Sys.time()
            checkpoint_payload$metadata$convergence <- conv_stat
            last_checkpoint_path <- save_saom_checkpoint(
              checkpoint_dir,
              model_id,
              candidate,
              checkpoint_payload$metadata,
              filename = basename(last_checkpoint_path),
              tz = timezone
            )
          }
        }
        break
      }
    }

    if (inherits(result, "try-error")) {
      status <- list(
        model = model_id,
        status = "estimation_failed",
        requested_at = format(Sys.time(), tz = config$project$timezone %||% "UTC"),
        specification_file = spec_path,
        fit_path = NULL,
        error = conditionMessage(attr(result, "condition")),
        skipped_network_effects = effects_info$skipped$network,
        skipped_behaviour_effects = effects_info$skipped$behaviour,
        actor_covariates = dataset_info$actor_covariates,
        dyadic_covariates = dataset_info$dyadic_covariates
      )

      if (!is.null(run_timing)) {
        status$runtime <- run_timing
      }

      log_path <- file.path(logs_dir, sprintf("saom_run_%s.json", model_id))
      jsonlite::write_json(status, log_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
      message("SAOM estimation could not be completed; see log for details.")
      return(invisible(NULL))
    }

    save(result, dataset_info, effects_info, file = fit_path)
  } else if (!file.exists(fit_path)) {
    save(result, dataset_info, effects_info, file = fit_path)
  }

  placeholder_fit <- is_placeholder_fit(result)

  convergence <- if (!is.null(result$tconv.max)) max(abs(result$tconv.max)) else NA_real_
  phase_reached <- result$phase
  summary_obj <- tryCatch({
    summary(result)
  }, error = function(err) NULL)

  theta_entries <- NULL
  effect_names <- character(0)
  theta_matrix <- NULL

  # Primary extraction: pull estimates and SEs directly from the sienaFit object.
  # RSiena stores result$theta (estimates vector) and result$se (SE vector) on the
  # fit; summary()$theta is unreliable across versions and may be NULL or a vector.
  direct_theta <- tryCatch(as.numeric(result$theta), error = function(err) NULL)
  direct_se    <- tryCatch(as.numeric(result$se),    error = function(err) NULL)

  if (!is.null(direct_theta) && length(direct_theta) > 0) {
    n_eff <- length(direct_theta)
    if (is.null(direct_se) || length(direct_se) != n_eff) {
      direct_se <- rep(NA_real_, n_eff)
    }
    theta_matrix <- cbind(estimate = direct_theta, std_error = direct_se)

    # Effect names: prefer result$effects$effectName, fall back to summary
    effect_names <- tryCatch(result$effects$effectName, error = function(err) NULL)
    if (is.null(effect_names) || length(effect_names) != n_eff) {
      effect_names <- tryCatch(summary_obj$effects$effectName, error = function(err) NULL)
    }
    if (is.null(effect_names) || length(effect_names) != n_eff) {
      effect_names <- paste0("effect_", seq_len(n_eff))
    }
    rownames(theta_matrix) <- effect_names

    theta_entries <- lapply(seq_len(n_eff), function(idx) {
      list(
        effect = effect_names[idx],
        estimate = direct_theta[idx]
      )
    })
  } else if (!is.null(summary_obj)) {
    # Fallback: try summary_obj$theta (works for placeholder fits)
    theta_matrix <- tryCatch(normalize_theta_matrix(summary_obj$theta), error = function(err) NULL)
    if (!is.null(theta_matrix) && nrow(theta_matrix) > 0) {
      effect_names <- rownames(theta_matrix)
      if (is.null(effect_names) || length(effect_names) == 0 || all(!nzchar(effect_names))) {
        candidate_names <- tryCatch(summary_obj$effects$effectName, error = function(err) NULL)
        if (!is.null(candidate_names) && length(candidate_names) >= nrow(theta_matrix)) {
          effect_names <- candidate_names[seq_len(nrow(theta_matrix))]
        } else {
          effect_names <- paste0("effect_", seq_len(nrow(theta_matrix)))
        }
        rownames(theta_matrix) <- effect_names
      }

      theta_entries <- lapply(seq_len(nrow(theta_matrix)), function(idx) {
        list(
          effect = effect_names[idx],
          estimate = as.numeric(theta_matrix[idx, 1])
        )
      })
    }
  }

  map_target_effects <- function(key) {
    default_map <- list(
      peer_influence = c("SAOM_behaviour average similarity", "average similarity", "avSim"),
      behaviour_linear_shape = c("SAOM_behaviour linear shape", "linear shape", "linear"),
      alcohol_use_global = c("SAOM_behaviour linear shape", "linear shape", "linear")
    )
    default_map[[key]] %||% c(key)
  }

  target_details <- NULL
  if (!is.null(chapter_cfg$target_coefficients) && length(effect_names) > 0) {
    target_details <- lapply(names(chapter_cfg$target_coefficients), function(key) {
      patterns <- map_target_effects(key)
      match_idx <- integer(0)
      for (pattern in patterns) {
        match_idx <- which(grepl(pattern, effect_names, fixed = TRUE))
        if (length(match_idx)) {
          break
        }
      }
      estimate <- if (length(match_idx) > 0 && !is.null(theta_entries)) {
        theta_entries[[match_idx[1]]]$estimate
      } else {
        NA_real_
      }
      list(
        metric = key,
        effect = if (length(match_idx) > 0) effect_names[match_idx[1]] else NULL,
        target = chapter_cfg$target_coefficients[[key]],
        estimate = if (is.na(estimate)) NULL else estimate,
        delta = if (is.na(estimate)) NULL else estimate - chapter_cfg$target_coefficients[[key]]
      )
    })
  }

  diagnostics_config <- spec$defaults$diagnostics %||% list()
  diagnostics_config <- merge_lists(diagnostics_config, config$rsiena$diagnostics %||% list())
  diagnostics_config <- merge_lists(diagnostics_config, model_spec$diagnostics %||% list())

  target_tolerance <- diagnostics_config$target_tolerance %||% chapter_cfg$target_tolerance %||% 0.1
  if (!is.null(target_details)) {
    target_details <- lapply(target_details, function(entry) {
      if (!is.null(entry$delta)) {
        entry$within_tolerance <- abs(entry$delta) <= target_tolerance
      }
      entry
    })
  }

  minimum_phase <- diagnostics_config$minimum_phase %||% NA_real_
  tolerance <- diagnostics_config$convergence_tolerance %||% chapter_cfg$convergence_tolerance %||% 0.1
  has_converged <- !is.na(convergence) && convergence <= tolerance
  phase_ok <- is.na(minimum_phase) || is.null(phase_reached) || phase_reached >= minimum_phase
  targets_ok <- TRUE
  if (!is.null(target_details)) {
    targets_ok <- all(vapply(target_details, function(entry) isTRUE(entry$within_tolerance), logical(1)))
  }

  convergence_trace <- NULL
  if (!is.null(result$tconv)) {
    if (is.matrix(result$tconv)) {
      convergence_trace <- as.numeric(apply(abs(result$tconv), 1, max))
    } else if (is.numeric(result$tconv)) {
      convergence_trace <- as.numeric(result$tconv)
    }
  }

  diagnostics_summary <- list(
    convergence = list(
      max = if (is.na(convergence)) NULL else convergence,
      tolerance = tolerance,
      within_tolerance = has_converged,
      trace = convergence_trace
    ),
    phase = list(
      reached = phase_reached,
      minimum = if (is.na(minimum_phase)) NULL else minimum_phase,
      within_tolerance = phase_ok
    )
  )

  if (!is.null(target_details)) {
    diagnostics_summary$targets <- target_details
    diagnostics_summary$target_tolerance <- target_tolerance
  }

  diagnostic_failures <- character(0)
  if (!has_converged) {
    if (is.na(convergence)) {
      diagnostic_failures <- c(diagnostic_failures, "convergence statistic unavailable")
    } else {
      diagnostic_failures <- c(diagnostic_failures, sprintf("convergence max %.4f exceeds tolerance %.4f", convergence, tolerance))
    }
  }
  if (!phase_ok) {
    diagnostic_failures <- c(diagnostic_failures, sprintf("phase %s below minimum %s", phase_reached %||% "<unknown>", minimum_phase))
  }
  if (!targets_ok) {
    failing_targets <- vapply(Filter(function(entry) !isTRUE(entry$within_tolerance), target_details), function(entry) entry$metric, character(1))
    diagnostic_failures <- c(diagnostic_failures, sprintf("targets outside tolerance: %s", paste(failing_targets, collapse = ", ")))
  }

  status_label <- if (length(diagnostic_failures) == 0) "converged" else "diagnostic_failure"
  run_failures <- diagnostic_failures

  status <- list(
    model = model_id,
    status = status_label,
    requested_at = format(Sys.time(), tz = config$project$timezone %||% "UTC"),
    specification_file = spec_path,
    fit_path = fit_path,
    phase_reached = phase_reached,
    convergence_tolerance = tolerance,
    tconv_max = convergence,
    cache_strategy = cache_strategy,
    skipped_network_effects = effects_info$skipped$network,
    skipped_behaviour_effects = effects_info$skipped$behaviour,
    actor_covariates = dataset_info$actor_covariates,
    dyadic_covariates = dataset_info$dyadic_covariates,
    dyadic_nonzero_counts = as.list(dataset_info$dyadic_nonzero_counts),
    estimation_mode = estimation_mode,
    data_warnings = if (length(data_warnings) > 0) data_warnings else NULL,
    network_ties = list(
      observed = if (is.na(network_tie_count)) NULL else network_tie_count,
      possible = if (is.na(total_possible_ties)) NULL else total_possible_ties
    )
  )

  input_details <- list(
    placeholder = placeholder_inputs
  )
  if (!is.null(synthetic_source) && nzchar(synthetic_source)) {
    input_details$synthetic_source <- synthetic_source
  }
  if (!is.null(payload_metadata$list_by_wave_path)) {
    input_details$list_by_wave_path <- payload_metadata$list_by_wave_path
  }
  if (!is.null(payload_metadata$statistics)) {
    input_details$statistics <- payload_metadata$statistics
  }
  status$inputs <- input_details

  if (resume_enabled) {
    status$resume <- list(
      enabled = TRUE,
      batches_completed = resume_batches_completed,
      checkpoints_written = checkpoints_written,
      resumed_from = resumed_from_path,
      last_checkpoint = last_checkpoint_path,
      checkpoint_dir = checkpoint_dir,
      mode = estimation_mode
    )
    if (!is.null(status$resume$checkpoint_dir) && exists("relative_repo_path", mode = "function")) {
      status$resume$checkpoint_dir <- relative_repo_path(status$resume$checkpoint_dir, repo_root)
    }
    if (!is.null(status$resume$resumed_from) && exists("relative_repo_path", mode = "function")) {
      status$resume$resumed_from <- relative_repo_path(status$resume$resumed_from, repo_root)
    }
    if (!is.null(status$resume$last_checkpoint) && exists("relative_repo_path", mode = "function")) {
      status$resume$last_checkpoint <- relative_repo_path(status$resume$last_checkpoint, repo_root)
    }
  } else {
    status$resume <- list(enabled = FALSE)
  }

  status$diagnostics <- diagnostics_summary
  status$rng <- list(
    project_seed = config$rsiena$project_seed,
    algorithm_seed = algorithm_params$seed,
    settings = get_rng_settings()
  )

  if (!is.null(run_timing)) {
    status$runtime <- run_timing
    base_log_path <- file.path(logs_dir, "saom_run_base.json")
    base_log <- list(
      model = model_id,
      estimation_mode = estimation_mode,
      started_at = run_timing$started_at,
      finished_at = run_timing$finished_at,
      duration_seconds = run_timing$duration_seconds,
      rng_seed = run_timing$rng_seed,
      convergence = diagnostics_summary$convergence
    )
    jsonlite::write_json(base_log, base_log_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  }

  if (placeholder_fit) {
    status$placeholder <- TRUE
  }

  if (!is.null(theta_entries)) {
    status$theta <- theta_entries
  }

  if (!is.null(chapter_cfg$target_coefficients)) {
    status$target_coefficients <- chapter_cfg$target_coefficients
  }

  if (!is.null(target_details)) {
    status$target_diagnostics <- target_details
    status$target_tolerance <- target_tolerance
  }

  if (!is.null(cached_source)) {
    status$cached_source <- cached_source
  }

  downstream_outputs <- list()
  downstream_runs <- list()
  if (length(diagnostic_failures) == 0) {
    tables_dir <- file.path(outputs_dir, "tables")
    validations_dir <- file.path(outputs_dir, "validations")
    dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(validations_dir, recursive = TRUE, showWarnings = FALSE)
    downstream_failure_messages <- character(0)

    if (!is.null(summary_obj) && !is.null(theta_matrix) && nrow(theta_matrix) > 0) {
      coef_df <- data.frame(
        effect = rownames(theta_matrix),
        estimate = as.numeric(theta_matrix[, 1]),
        stringsAsFactors = FALSE
      )
      if (ncol(theta_matrix) >= 2) {
        coef_df$std_error <- as.numeric(theta_matrix[, 2])
      }
      if (ncol(theta_matrix) >= 3) {
        coef_df$p_value <- as.numeric(theta_matrix[, 3])
      }

      coefficients_path <- file.path(tables_dir, sprintf("saom_coefficients_%s.csv", model_id))
      write_csv_safely(coef_df, coefficients_path)
      downstream_outputs$coefficient_table <- if (exists("relative_repo_path", mode = "function")) {
        relative_repo_path(coefficients_path, repo_root)
      } else {
        coefficients_path
      }
    }

    validation_payload <- list(
      model = model_id,
      generated_at = status$requested_at,
      diagnostics = diagnostics_summary,
      rng = status$rng,
      fit = list(
        path = status$fit_path,
        cached = identical(estimation_mode, "cached")
      )
    )
    validation_path <- file.path(validations_dir, sprintf("saom_validation_%s.json", model_id))
    jsonlite::write_json(validation_payload, validation_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
    downstream_outputs$validation_report <- if (exists("relative_repo_path", mode = "function")) {
      relative_repo_path(validation_path, repo_root)
    } else {
      validation_path
    }

    downstream_scripts <- chapter_cfg$downstream_scripts %||% config$rsiena$diagnostics$downstream_scripts
    if (!is.null(downstream_scripts)) {
      for (script_rel in downstream_scripts) {
        script_path <- tryCatch(resolve_repo_path(repo_root, script_rel, must_work = FALSE), error = function(e) NULL)
        if (is.null(script_path) || !nzchar(script_path) || !file.exists(script_path)) {
          downstream_runs <- c(downstream_runs, list(list(
            script = script_rel,
            status = "missing"
          )))
          downstream_failure_messages <- c(downstream_failure_messages, sprintf("downstream script missing: %s", script_rel))
          next
        }

        if (grepl("\\.R$", script_path, ignore.case = TRUE)) {
          args <- c("--vanilla", script_path, "--model", model_id)
          run_result <- try(system2("Rscript", args, stdout = TRUE, stderr = TRUE), silent = TRUE)
        } else {
          args <- c("--model", model_id)
          run_result <- try(system2(script_path, args, stdout = TRUE, stderr = TRUE), silent = TRUE)
        }
        if (inherits(run_result, "try-error")) {
          downstream_runs <- c(downstream_runs, list(list(
            script = script_rel,
            status = "error",
            message = conditionMessage(attr(run_result, "condition"))
          )))
          downstream_failure_messages <- c(
            downstream_failure_messages,
            sprintf(
              "downstream script error: %s (%s)",
              script_rel,
              conditionMessage(attr(run_result, "condition"))
            )
          )
        } else {
          status_code <- attr(run_result, "status")
          downstream_runs <- c(downstream_runs, list(list(
            script = script_rel,
            status = if (is.null(status_code) || identical(status_code, 0L)) "completed" else sprintf("exit_%s", status_code)
          )))
          if (!is.null(status_code) && !identical(status_code, 0L)) {
            downstream_failure_messages <- c(
              downstream_failure_messages,
              sprintf("downstream script exited with status %s: %s", status_code, script_rel)
            )
          }
        }
      }
    }

    if (length(downstream_failure_messages) > 0) {
      run_failures <- c(run_failures, downstream_failure_messages)
    }
  }

  if (length(downstream_outputs) > 0) {
    status$downstream_outputs <- downstream_outputs
  }
  if (length(downstream_runs) > 0) {
    status$downstream_runs <- downstream_runs
  }

  if (length(run_failures) > 0) {
    status$diagnostic_failures <- run_failures
    if (length(diagnostic_failures) == 0) {
      status$status <- "downstream_failure"
    }
    status$degraded <- TRUE
  }

  log_path <- file.path(logs_dir, sprintf("saom_run_%s.json", model_id))
  jsonlite::write_json(status, log_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  if (length(run_failures) > 0) {
    convergence_note <- NULL
    convergence_max <- status$diagnostics$convergence$max
    convergence_tol <- status$diagnostics$convergence$tolerance
    if (!is.null(convergence_max)) {
      convergence_note <- sprintf("max convergence %.4f (tolerance %.4f)", convergence_max, convergence_tol %||% NA_real_)
    }
    details <- paste(run_failures, collapse = "; ")
    if (!is.null(convergence_note)) {
      details <- paste(convergence_note, details, sep = "; ")
    }
    message(sprintf(
      "SAOM model '%s' diagnostics flagged issues; details recorded at %s: %s",
      model_id,
      log_path,
      details
    ))
    return(invisible(status))
  }

  message(sprintf("SAOM model '%s' estimation completed. Results stored at %s", model_id, fit_path))
  invisible(status)
}

if (identical(environment(), globalenv())) {
  debug_mode <- identical(Sys.getenv("SAOM_DEBUG"), "1")
  tryCatch(
    main(),
    error = function(err) {
      message("Error running SAOM model: ", err$message)
      if (debug_mode) {
        stop(err)
      }
      quit(status = 1)
    }
  )
}
