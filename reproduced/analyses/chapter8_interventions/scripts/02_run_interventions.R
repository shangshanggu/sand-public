#!/usr/bin/env Rscript
# 02_run_interventions.R
#
# First-period ("within") forward simulation.  For each row in the expanded
# strategy grid this script:
#   1. Initialises the intervention wave's observed network + behaviour
#   2. Selects targets via the targeting engine (IT/ST/CT strategies)
#   3. Applies the intervention efficacy to targeted actors' AUDIT-C scores
#   4. Runs RSiena siena07() in simOnly mode (n3 replications, seed=2022)
#   5. Collects mean/sd/distribution statistics across all replications
#
# Ported from the original sim_clean/ scripts:
#   0_baseline_setup.R, 1_intervention_initialisation.R,
#   2_intervention_implementation.R, 3_intervention_siena_run1.R,
#   z_intervention_wrapper_1stwithin.R
#
# Outputs:
#   - reproduced/outputs/chapter8/results/df_results_1st.csv
#   - reproduced/outputs/chapter8/cache/list_initsim_1st.RData
#   - reproduced/outputs/chapter8/cache/list_rank_1st.RData

suppressPackageStartupMessages({
library(yaml)
library(jsonlite)
library(igraph)
library(abind)
library(parallel)
})

# ---------------------------------------------------------------------------
# Debug helper: timestamped messages with memory + elapsed tracking
# ---------------------------------------------------------------------------
.ch8_start_time <- proc.time()["elapsed"]

ch8_debug <- function(..., level = "INFO") {
  elapsed <- proc.time()["elapsed"] - .ch8_start_time
  mem_mb  <- tryCatch(
    round(as.numeric(gc(verbose = FALSE)[2, 2]), 1),
    error = function(e) NA_real_
  )
  ts <- format(Sys.time(), "%H:%M:%S")
  prefix <- sprintf("[ch8:%s %s +%.0fs mem=%.0fMB]", level, ts, elapsed, mem_mb)
  message(prefix, " ", paste0(...))
}

ch8_debug_scenario <- function(idx, total, row, phase, extra = "") {
  ch8_debug(sprintf("scenario %d/%d | type=%s wave=%d tgt=%s prop=%s eff=%s | %s%s",
    idx, total,
    row$intervention_type, row$intervention_wave,
    row$intervention_targeting,
    ifelse(is.na(row$intervention_proportion), "NA", row$intervention_proportion),
    ifelse(is.na(row$intervention_efficacy), "NA", row$intervention_efficacy),
    phase, extra))
}

script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grep(file_arg, args)])
  if (length(script_path) == 0) return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  normalizePath(dirname(script_path[1]), winslash = "/", mustWork = TRUE)
}

.resolve_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  if (any(grepl("^--file=", args))) {
    return(normalizePath(file.path(script_dir(), "..", "..", "..", ".."),
                         winslash = "/", mustWork = TRUE))
  }
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (dir.exists(file.path(cwd, "reproduced"))) return(cwd)
  normalizePath(file.path(cwd, ".."), winslash = "/", mustWork = TRUE)
}
repo_root <- .resolve_repo_root()
source(file.path(repo_root, "reproduced", "R", "common.R"))

`%||%` <- function(x, y) if (is.null(x) || (is.character(x) && length(x) == 1L && !nzchar(x))) y else x

# ===========================================================================
# CLI
# ===========================================================================
parse_args <- function(args) {
  defaults <- list(
    config   = file.path(repo_root, "reproduced", "config", "thesis.yml"),
    max_runs = NA_integer_,
    cores    = NA_integer_,
    dry_run  = FALSE,
    resume   = TRUE,
    clean    = FALSE
  )
  out <- defaults
  i <- 1L
  while (i <= length(args)) {
    flag <- args[[i]]
    if (flag == "--config"   && i < length(args)) { out$config   <- args[[i+1]]; i <- i+2L; next }
    if (flag == "--max-runs" && i < length(args)) { out$max_runs <- as.integer(args[[i+1]]); i <- i+2L; next }
    if (flag == "--cores"    && i < length(args)) { out$cores    <- as.integer(args[[i+1]]); i <- i+2L; next }
    if (flag == "--dry-run")                      { out$dry_run  <- TRUE; i <- i+1L; next }
    if (flag == "--no-resume")                    { out$resume   <- FALSE; i <- i+1L; next }
    if (flag == "--clean")                        { out$clean    <- TRUE; out$resume <- FALSE; i <- i+1L; next }
    if (flag %in% c("-h","--help")) {
      cat("Usage: 02_run_interventions.R [--config PATH] [--max-runs N] [--cores N] [--dry-run] [--no-resume] [--clean]\n")
      cat("  --no-resume  Ignore previous checkpoint, re-run all scenarios\n")
      cat("  --clean      Delete previous results before starting\n")
      quit(status = 0L, save = "no")
    }
    stop(sprintf("Unknown option '%s'", flag), call. = FALSE)
  }
  out
}

# ===========================================================================
# Targeting engine  (ported from 2_intervention_implementation.R)
# ===========================================================================
select_targets <- function(df_rank, targeting, proportion, observed_network) {
  n <- nrow(df_rank)
  df_rank$intervention_chosen       <- FALSE
  df_rank$intervention_bringasfriend <- FALSE
  df_rank$intervention_flag          <- FALSE

  if (is.na(targeting)) return(df_rank)

  num_select <- ceiling(n * proportion / 100)

  if (targeting == "CT-RS") {
    # Random selection
    df_rank$intervention_chosen[sample(n, min(num_select, n))] <- TRUE

  } else if (targeting == "CT-HD") {
    # Heavy drinkers (highest AUDIT-C)
    df_rank <- df_rank[order(-df_rank$value_auditc), ]
    df_rank$intervention_chosen[seq_len(min(num_select, n))] <- TRUE

  } else if (targeting == "IT-ID") {
    # Individual targeting: highest indegree
    df_rank <- df_rank[order(-df_rank$value_indegree), ]
    df_rank$intervention_chosen[seq_len(min(num_select, n))] <- TRUE

  } else if (targeting == "IT-NC") {
    # Individual targeting: highest betweenness centrality
    df_rank <- df_rank[order(-df_rank$value_centrality), ]
    df_rank$intervention_chosen[seq_len(min(num_select, n))] <- TRUE

  } else if (targeting == "ST-ID") {
    # Segmentation: half coverage by indegree, each brings a friend
    num_half <- ceiling(n * proportion / 200)
    df_rank <- df_rank[order(-df_rank$value_indegree), ]
    eligible <- which(df_rank$value_outdegree[seq_len(min(num_half, n))] > 0)
    df_rank$intervention_chosen[eligible] <- TRUE

  } else if (targeting == "ST-NC") {
    # Segmentation: half coverage by centrality, each brings a friend
    num_half <- ceiling(n * proportion / 200)
    df_rank <- df_rank[order(-df_rank$value_centrality), ]
    eligible <- which(df_rank$value_outdegree[seq_len(min(num_half, n))] > 0)
    df_rank$intervention_chosen[eligible] <- TRUE
  }

  # Bring-a-friend logic for ST strategies
  if (!is.na(proportion) && proportion > 0 && targeting %in% c("ST-ID", "ST-NC")) {
    adj <- as.matrix(igraph::as_adjacency_matrix(observed_network))
    for (i in which(df_rank$intervention_chosen)) {
      friends <- which(adj[i, ] == 1)
      if (length(friends) > 0) {
        friend <- sample(friends, 1)
        df_rank$intervention_bringasfriend[friend] <- TRUE
      }
    }
    df_rank$intervention_flag <- df_rank$intervention_chosen | df_rank$intervention_bringasfriend
  } else {
    df_rank$intervention_flag <- df_rank$intervention_chosen
  }

  # 0% proportion = no intervention

  if (!is.na(proportion) && proportion == 0) {
    df_rank$intervention_flag <- FALSE
  }

  df_rank
}

# ===========================================================================
# Apply intervention efficacy to behaviour
# ===========================================================================
apply_intervention <- function(behaviour_vec, flags, base_efficacy, efficacy_pct,
                               intervention_type, parameters = NULL) {
  updated <- behaviour_vec
  updated[flags] <- updated[flags] * (1 + base_efficacy * efficacy_pct / 100)
  updated <- pmax(0, round(updated))

  # Type C also scales the avSim parameter
  updated_params <- parameters
  if (!is.null(parameters) && intervention_type == "C") {
    avsim_row <- which(parameters[, 1] == "SAOM_behaviour average similarity")
    if (length(avsim_row) > 0) {
      updated_params[avsim_row, 2] <- as.numeric(parameters[avsim_row, 2]) *
        (1 + base_efficacy * efficacy_pct / 100)
    }
  }

  list(behaviour = as.integer(updated), parameters = updated_params)
}

# ===========================================================================
# Build RSiena data + effects for forward simulation
# Constructs the sienaDataCreate object from first principles using the
# network_arrays.rds payload — no legacy functions needed.
#
# Parameters:
#   updated_behaviour  - integer vector of (possibly modified) AUDIT-C scores
#   next_wave_behaviour - integer vector of next-wave observed AUDIT-C scores
#   network_current    - adjacency matrix for the intervention wave
#   network_next       - adjacency matrix for the simulated (next) wave
#   actor_covariates   - data.frame with majority_status and sex columns
#   dyadic_covariates  - list with flatmates and blockmates matrices
#   updated_parameters - 2-column matrix (effects, value) from baseline params
#   input_nthree       - number of simulation replications
#   cores              - number of parallel cores
# ===========================================================================
build_siena_forward_sim <- function(updated_behaviour, next_wave_behaviour,
                                    network_current, network_next,
                                    actor_covariates, dyadic_covariates,
                                    updated_parameters,
                                    input_nthree, cores) {
  # Network: 3D array (current + next), 2 waves for 1 period
  net_current <- as.matrix(network_current)
  net_next    <- as.matrix(network_next)
  n <- nrow(net_current)
  net_arr <- array(0L, dim = c(n, n, 2))
  net_arr[, , 1] <- net_current
  net_arr[, , 2] <- net_next

  # Behaviour: 2-column matrix (current updated + next observed)
  beh_mat <- cbind(as.integer(updated_behaviour), as.integer(next_wave_behaviour))

  # RSiena dependent variables
  SAOM_friendship <- RSiena::sienaDependent(net_arr, type = "oneMode", allowOnly = FALSE)
  SAOM_behaviour  <- RSiena::sienaDependent(beh_mat, type = "behavior")

  # Actor covariates
  SAOM_majority_status <- RSiena::coCovar(as.numeric(actor_covariates$majority_status))
  SAOM_sex             <- RSiena::coCovar(as.numeric(actor_covariates$sex))

  # Dyadic covariates
  SAOM_flatmates  <- RSiena::coDyadCovar(as.matrix(dyadic_covariates$flatmates))
  SAOM_blockmates <- RSiena::coDyadCovar(as.matrix(dyadic_covariates$blockmates))

  # Build data object from first principles (replaces legacy fun_creating_SAOM_data)
  mydata <- RSiena::sienaDataCreate(
    SAOM_friendship,
    SAOM_behaviour,
    majority_status = SAOM_majority_status,
    sex             = SAOM_sex,
    flatmates       = SAOM_flatmates,
    blockmates      = SAOM_blockmates
  )

  # Algorithm: simulation only, no estimation
  InitAlg <- RSiena::sienaAlgorithmCreate(
    projname     = "Init",
    useStdInits  = FALSE,
    cond         = FALSE,
    nsub         = 0,
    n3           = input_nthree,
    simOnly      = TRUE,
    MaxDegree    = c(SAOM_friendship = 10),
    seed         = 2022
  )

  # Effects: map parameter estimates by effect name (not hardcoded row index)
  myeff <- RSiena::getEffects(mydata)

  # Build a lookup from effect name → estimated value
  param_lookup <- setNames(as.double(updated_parameters[, 2]),
                           as.character(updated_parameters[, 1]))

  # Helper: find parameter value by matching effect name substring
  pval <- function(pattern) {
    idx <- grep(pattern, names(param_lookup), fixed = TRUE)
    if (length(idx) == 0) stop(sprintf("No parameter matching '%s'", pattern), call. = FALSE)
    param_lookup[idx[1]]
  }

  # A. Structural network effects
  myeff <- RSiena::setEffect(myeff, density,      initialValue = pval("outdegree (density)"))
  myeff <- RSiena::setEffect(myeff, recip,         initialValue = pval("reciprocity"))
  myeff <- RSiena::setEffect(myeff, transTrip,     initialValue = pval("transitive triplets"))
  myeff <- RSiena::setEffect(myeff, transRecTrip,  initialValue = pval("transitive recipr. triplets"))
  myeff <- RSiena::setEffect(myeff, inPop,         initialValue = pval("indegree - popularity"))
  myeff <- RSiena::setEffect(myeff, outPop,        initialValue = pval("outdegree - popularity"))
  myeff <- RSiena::setEffect(myeff, outAct,        initialValue = pval("outdegree - activity"))

  # B. Dyadic covariates
  myeff <- RSiena::setEffect(myeff, X, interaction1 = "flatmates",  initialValue = pval("flatmates"))
  myeff <- RSiena::setEffect(myeff, X, interaction1 = "blockmates", initialValue = pval("blockmates"))

  # C. Actor covariates on network (majority_status)
  myeff <- RSiena::setEffect(myeff, altX, interaction1 = "majority_status", initialValue = pval("majority_status alter"))
  myeff <- RSiena::setEffect(myeff, egoX, interaction1 = "majority_status", initialValue = pval("majority_status ego"))
  myeff <- RSiena::setEffect(myeff, simX, interaction1 = "majority_status", initialValue = pval("majority_status similarity"))

  # D. Actor covariates on network (sex)
  myeff <- RSiena::setEffect(myeff, altX, interaction1 = "sex", initialValue = pval("sex alter"))
  myeff <- RSiena::setEffect(myeff, egoX, interaction1 = "sex", initialValue = pval("sex ego"))
  myeff <- RSiena::setEffect(myeff, simX, interaction1 = "sex", initialValue = pval("sex similarity"))

  # E. Selection effects (behaviour on network)
  myeff <- RSiena::setEffect(myeff, egoX, name = "SAOM_friendship", interaction1 = "SAOM_behaviour",
                             initialValue = pval("SAOM_behaviour ego"))
  myeff <- RSiena::setEffect(myeff, altX, name = "SAOM_friendship", interaction1 = "SAOM_behaviour",
                             initialValue = pval("SAOM_behaviour alter"))
  myeff <- RSiena::setEffect(myeff, simX, name = "SAOM_friendship", interaction1 = "SAOM_behaviour",
                             initialValue = pval("SAOM_behaviour similarity"))

  # F. Behaviour shape effects
  myeff <- RSiena::setEffect(myeff, linear, name = "SAOM_behaviour", initialValue = pval("SAOM_behaviour linear shape"))
  myeff <- RSiena::setEffect(myeff, quad,   name = "SAOM_behaviour", initialValue = pval("SAOM_behaviour quadratic shape"))

  # G. Influence effects (network on behaviour)
  myeff <- RSiena::setEffect(myeff, avSim,  name = "SAOM_behaviour", interaction1 = "SAOM_friendship",
                             initialValue = pval("SAOM_behaviour average similarity"))
  myeff <- RSiena::setEffect(myeff, indeg,  name = "SAOM_behaviour", interaction1 = "SAOM_friendship",
                             initialValue = pval("SAOM_behaviour indegree"))
  myeff <- RSiena::setEffect(myeff, outdeg, name = "SAOM_behaviour", interaction1 = "SAOM_friendship",
                             initialValue = pval("SAOM_behaviour outdegree"))

  # H. Covariate effects on behaviour
  myeff <- RSiena::setEffect(myeff, effFrom, name = "SAOM_behaviour", interaction1 = "majority_status",
                             initialValue = pval("effect from majority_status"))
  myeff <- RSiena::setEffect(myeff, effFrom, name = "SAOM_behaviour", interaction1 = "sex",
                             initialValue = pval("effect from sex"))

  # Run forward simulation
  # If a pre-made cluster is passed, use it; otherwise let siena07 create one
  if (!is.null(cores) && inherits(cores, "cluster")) {
    InitSim <- RSiena::siena07(InitAlg, data = mydata, eff = myeff,
                               batch = TRUE, verbose = FALSE,
                               cl = cores,
                               returnDeps = TRUE)
  } else {
    n_cores <- if (is.numeric(cores)) cores else parallel::detectCores()
    InitSim <- RSiena::siena07(InitAlg, data = mydata, eff = myeff,
                               batch = TRUE, verbose = FALSE,
                               useCluster = TRUE, nbrNodes = n_cores,
                               returnDeps = TRUE)
  }
  InitSim
}

# ===========================================================================
# Collect statistics from simulation replications
# ===========================================================================
collect_sim_stats <- function(InitSim) {
  n_sims <- length(InitSim$sims)
  mean_vals <- sd_vals <- n0 <- n4 <- n8 <- n12 <- numeric(n_sims)

  for (s in seq_len(n_sims)) {
    beh <- InitSim$sims[[s]]$Data1$SAOM_behaviour[[1]]
    n_actors <- length(beh)
    mean_vals[s] <- mean(beh)
    sd_vals[s]   <- sd(beh)
    n0[s]  <- sum(beh == 0) / n_actors
    n4[s]  <- sum(beh > 0 & beh <= 4) / n_actors
    n8[s]  <- sum(beh > 4 & beh <= 8) / n_actors
    n12[s] <- sum(beh > 8) / n_actors
  }

  # Peak run: closest to KDE mode of mean values
  dens <- density(mean_vals)
  peak_idx <- which.min(abs(mean_vals - dens$x[which.max(dens$y)]))
  peak_beh <- InitSim$sims[[peak_idx]]$Data1$SAOM_behaviour[[1]]
  last_beh <- InitSim$sims[[n_sims]]$Data1$SAOM_behaviour[[1]]

  beh_stats <- function(b, prefix) {
    n <- length(b)
    setNames(c(mean(b), sd(b),
               sum(b == 0)/n, sum(b > 0 & b <= 4)/n,
               sum(b > 4 & b <= 8)/n, sum(b > 8)/n),
             paste0(prefix, c("mean_value", "sd_value",
                              "n_non_drinker", "n_1_to_4", "n_5_to_8", "n_9_to_12")))
  }

  c(mean_value     = mean(mean_vals),
    sd_value        = mean(sd_vals),
    n_non_drinker   = mean(n0),
    n_1_to_4        = mean(n4),
    n_5_to_8        = mean(n8),
    n_9_to_12       = mean(n12),
    peak_run_index  = peak_idx,
    beh_stats(peak_beh, "peak_run_"),
    beh_stats(last_beh, "last_run_"))
}

# ===========================================================================
# Main loop
# ===========================================================================
run <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  cfg  <- yaml::read_yaml(args$config)
  int_cfg <- yaml::read_yaml(file.path(repo_root, "reproduced", "config",
                                        "scenarios", "interventions.yml"))

  ch8_debug("=== Chapter 8 Step 02: First-period intervention simulations ===")
  ch8_debug("Config: ", args$config)
  ch8_debug("Dry-run: ", args$dry_run)
  ch8_debug("R version: ", R.version.string)
  ch8_debug("PID: ", Sys.getpid())
  ch8_debug("Working directory: ", getwd())

  # --- Ensure RSiena is available (same pattern as Ch7 02_run_saom_model.R) ---
  rsiena_install_cfg <- cfg$rsiena$installation %||% list()
  library_dir_relative <- rsiena_install_cfg$library_dir %||% NULL
  if (!is.null(library_dir_relative) && nzchar(library_dir_relative)) {
    library_dir <- normalizePath(file.path(repo_root, library_dir_relative),
                                 winslash = "/", mustWork = FALSE)
    if (dir.exists(library_dir)) {
      .libPaths(c(library_dir, .libPaths()))
      ch8_debug("Added RSiena library path: ", library_dir)
    } else {
      ch8_debug("RSiena library dir not found: ", library_dir, level = "WARN")
    }
  }

  if (!args$dry_run) {
    if (!requireNamespace("RSiena", quietly = TRUE)) {
      stop("RSiena package not found. Install via 'make rsiena' or check ",
           "rsiena.installation.library_dir in thesis.yml points to the correct path.\n",
           "Current .libPaths(): ", paste(.libPaths(), collapse = ", "),
           call. = FALSE)
    }
    rsiena_ver <- tryCatch(as.character(packageVersion("RSiena")), error = function(e) "unknown")
    ch8_debug("RSiena loaded successfully, version: ", rsiena_ver)
  }

  ch8_debug(".libPaths(): ", paste(.libPaths(), collapse = " ; "))
  logs_root <- file.path(repo_root, cfg$project$paths$logs_dir %||% "reproduced/logs")
  ch8_out   <- file.path(repo_root, cfg$chapters$chapter8_interventions$outputs_dir
                          %||% "reproduced/outputs/chapter8")

  # Wave mapping (thesis §8): survey waves → simulation target waves
  waves_survey <- c(2L, 4L, 5L, 6L)
  waves_period <- c(4L, 5L, 6L, NA_integer_)

  # Load prepared data
  df_strategy <- read.csv(file.path(ch8_out, "data", "df_intervention_strategy.csv"),
                          stringsAsFactors = FALSE)
  df_base_parameters <- readRDS(file.path(ch8_out, "data", "baseline_parameters.rds"))
  ch8_debug("Strategy grid: ", nrow(df_strategy), " scenarios, ",
            ncol(df_base_parameters), " parameter columns")
  ch8_debug("Strategy columns: ", paste(names(df_strategy), collapse = ", "))
  ch8_debug("Parameter effects: ", nrow(df_base_parameters), " effects")

  # Load the network_arrays.rds payload (produced by Ch7 00_build_network_arrays_base.R)
  # This contains everything we need: network arrays, behaviour, covariates
  network_arrays_path <- file.path(repo_root,
    cfg$chapters$chapter7_saom$required_inputs[[1]] %||%
    "reproduced/outputs/chapter4/data/network_arrays.rds")
  if (!file.exists(network_arrays_path)) {
    stop(sprintf("network_arrays.rds not found at %s.\nRun Chapter 7 preprocessing first.",
                 network_arrays_path), call. = FALSE)
  }
  payload <- readRDS(network_arrays_path)
  ch8_debug("Loaded network_arrays.rds from: ", network_arrays_path)

  # Extract what we need from the payload
  ids              <- payload$ids
  network_array    <- payload$network_array       # n × n × 4 (waves 2,4,5,6)
  behaviour_array  <- payload$behaviour_array     # n × 4
  actor_covariates <- payload$actor_covariates    # data.frame: majority_status, sex
  dyadic_covariates <- payload$dyadic_covariates  # list: flatmates, blockmates

  ch8_debug("Payload: ", length(ids), " actors, network_array dim=",
            paste(dim(network_array), collapse = "x"),
            ", behaviour_array dim=", paste(dim(behaviour_array), collapse = "x"))
  ch8_debug("Actor covariates: ", paste(names(actor_covariates), collapse = ", "))
  ch8_debug("Dyadic covariates: ", paste(names(dyadic_covariates), collapse = ", "))
  ch8_debug("Behaviour range: [", min(behaviour_array, na.rm = TRUE), ", ",
            max(behaviour_array, na.rm = TRUE), "]")
  ch8_debug("Network density per wave: ",
            paste(round(apply(network_array, 3, function(m) mean(m, na.rm = TRUE)), 4),
                  collapse = ", "))

  # Map survey waves to payload wave indices (payload has waves 2,4,5,6 as cols 1-4)
  wave_to_payload_idx <- setNames(1:4, c("2", "4", "5", "6"))

  # Base efficacies from config
  defaults <- int_cfg$defaults %||% list()
  base_eff <- defaults$base_efficacy %||% list()
  input_base_efficacy_A <- as.numeric(base_eff$type_a %||% -0.129)
  input_base_efficacy_B <- as.numeric(base_eff$type_b %||% -0.072)
  input_base_efficacy_C <- as.numeric(base_eff$type_c %||% -0.50)
  input_nthree <- as.integer(defaults$iterations %||% 1000)

  n_available <- detectCores()
  # Reserve 4 cores for the OS / other work; use at most 75% of available cores
  max_safe_cores <- max(2L, min(n_available - 4L, floor(n_available * 0.75)))
  cores <- if (!is.na(args$cores)) min(args$cores, n_available) else max_safe_cores
  message(sprintf("[cores] %d available, using %d (override with --cores N)", n_available, cores))
  max_runs <- if (is.na(args$max_runs)) nrow(df_strategy) else min(args$max_runs, nrow(df_strategy))

  ch8_debug("Scenario grid: ", max_runs, " scenarios to run")
  ch8_debug("n3 (replications per scenario): ", input_nthree)

  # =========================================================================
  # Resume support: load any previously completed results so we can skip
  # scenarios that already succeeded.  The checkpoint CSV is the source of
  # truth — if a row exists for scenario idx, we skip it.
  # =========================================================================
  results_csv_path <- file.path(ch8_out, "results", "df_results_1st.csv")
  cache_initsim_path <- file.path(ch8_out, "cache", "list_initsim_1st.RData")
  cache_rank_path    <- file.path(ch8_out, "cache", "list_rank_1st.RData")

  # Clean previous results if requested
  if (args$clean) {
    for (f in c(results_csv_path, cache_initsim_path, cache_rank_path)) {
      if (file.exists(f)) {
        file.remove(f)
        ch8_debug("Cleaned: ", f)
      }
    }
  }

  # Pre-allocate results list (avoid rbind-in-loop O(n²) copies)
  list_initsim <- vector("list", max_runs)
  list_rank    <- vector("list", max_runs)
  results_list <- vector("list", max_runs)
  completed    <- rep(FALSE, max_runs)

  # Try to load previous checkpoint
  if (args$resume && file.exists(results_csv_path)) {
    prev_results <- tryCatch(read.csv(results_csv_path, stringsAsFactors = FALSE),
                             error = function(e) NULL)
    if (!is.null(prev_results) && nrow(prev_results) > 0 &&
        "input_index_strategy" %in% names(prev_results)) {
      done_indices <- prev_results$input_index_strategy
      ch8_debug("Resume: found checkpoint with ", length(done_indices),
                " completed scenarios (indices: ",
                paste(range(done_indices), collapse = "-"), ")")
      for (di in done_indices) {
        if (di >= 1 && di <= max_runs) {
          results_list[[di]] <- prev_results[prev_results$input_index_strategy == di, , drop = FALSE]
          completed[[di]] <- TRUE
        }
      }
      # Also reload cached RData if available
      if (file.exists(cache_initsim_path) && !args$dry_run) {
        tryCatch({
          load(cache_initsim_path)  # loads list_initsim
          ch8_debug("Resume: reloaded list_initsim cache")
        }, error = function(e) {
          ch8_debug("Resume: could not reload list_initsim: ", e$message, level = "WARN")
          list_initsim <<- vector("list", max_runs)
        })
      }
      if (file.exists(cache_rank_path)) {
        tryCatch({
          load(cache_rank_path)  # loads list_rank
          ch8_debug("Resume: reloaded list_rank cache")
        }, error = function(e) {
          ch8_debug("Resume: could not reload list_rank: ", e$message, level = "WARN")
          list_rank <<- vector("list", max_runs)
        })
      }
    }
  }

  n_already_done <- sum(completed)
  n_remaining    <- max_runs - n_already_done
  ch8_debug("Scenarios: ", max_runs, " total, ", n_already_done, " already done, ",
            n_remaining, " remaining")

  if (n_remaining == 0) {
    ch8_debug("All scenarios already completed — nothing to do.")
    entry <- list(timestamp = format_timestamp(), action = "run_1st_period",
                  scenarios = max_runs, dry_run = args$dry_run,
                  resumed = TRUE, skipped = max_runs)
    append_pipeline_log(logs_root, "chapter8", entry, history_key = "intervention_runs")
    message(sprintf("First-period simulation complete (all resumed): %d scenarios", max_runs))
    return(invisible(NULL))
  }

  # Create a persistent PSOCK cluster once — reused across all scenarios
  cl <- NULL
  if (!args$dry_run) {
    ch8_debug("Creating PSOCK cluster with ", cores, " workers...")
    cl <- tryCatch({
      cluster <- parallel::makeCluster(cores)
      ch8_debug("Cluster created successfully, class=", class(cluster)[1])
      # Export RSiena library path to worker nodes so they can find the package
      rsiena_lib_paths <- .libPaths()
      parallel::clusterCall(cluster, function(paths) .libPaths(paths), rsiena_lib_paths)
      ch8_debug("Library paths exported to cluster workers")
      cluster
    }, error = function(e) {
      ch8_debug("FAILED to create cluster: ", e$message, level = "ERROR")
      stop("Cluster creation failed: ", e$message, call. = FALSE)
    })
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  message(sprintf("Running %d intervention scenarios (%d to do, %s mode, %d cores, n3=%d)",
                  max_runs, n_remaining,
                  if (args$dry_run) "dry-run" else "full", cores, input_nthree))

  # =========================================================================
  # Main scenario loop — with per-scenario error handling + checkpointing
  # =========================================================================
  scenarios_run <- 0L

  for (idx in seq_len(max_runs)) {
    # Skip already-completed scenarios
    if (completed[[idx]]) next

    t_scenario <- proc.time()["elapsed"]
    row <- df_strategy[idx, ]
    ch8_debug_scenario(idx, max_runs, row, "STARTING")

    # Wrap the entire scenario in tryCatch so we can checkpoint before dying
    scenario_result <- tryCatch({

      # --- 1. Initialisation (from 1_intervention_initialisation.R) ---
      intervention_wave <- as.integer(row$intervention_wave)
      wave_index <- which(waves_survey == intervention_wave)
      simulated_wave <- waves_period[wave_index]

      if (is.na(simulated_wave)) {
        ch8_debug("  Skipping: simulated_wave is NA (wave 6 has no next period)",
                  level = "WARN")
        return(list(status = "skipped", reason = "no_next_period"))
      }

      # Parameters for this period
      observed_parameters <- data.frame(
        effects = df_base_parameters$effects,
        value   = df_base_parameters[, wave_index + 1],
        stringsAsFactors = FALSE
      )

      # Get network and behaviour from the payload using wave indices
      payload_wave_idx <- wave_to_payload_idx[as.character(intervention_wave)]
      payload_sim_idx  <- wave_to_payload_idx[as.character(simulated_wave)]

      observed_network_mat <- network_array[, , payload_wave_idx]
      observed_audit_scores <- behaviour_array[, payload_wave_idx]

      observed_behaviour <- data.frame(
        id = ids,
        audit_score = observed_audit_scores,
        stringsAsFactors = FALSE
      )

      # --- 2. Implementation (from 2_intervention_implementation.R) ---
      g <- igraph::graph_from_adjacency_matrix(observed_network_mat, mode = "directed",
                                                diag = FALSE, weighted = NULL)

      df_rank <- data.frame(
        node_index = as.character(seq_len(nrow(observed_behaviour))),
        value_indegree   = igraph::degree(g, mode = "in"),
        value_centrality = igraph::betweenness(g),
        value_outdegree  = igraph::degree(g, mode = "out"),
        value_auditc     = observed_behaviour$audit_score,
        stringsAsFactors = FALSE
      )

      proportion <- ifelse(is.na(row$intervention_proportion), 0, row$intervention_proportion)
      df_rank <- select_targets(df_rank, row$intervention_targeting, proportion, g)

      base_eff_val <- switch(row$intervention_type,
        "A" = input_base_efficacy_A,
        "B" = input_base_efficacy_B,
        "C" = input_base_efficacy_C,
        0)
      efficacy_pct <- ifelse(is.na(row$intervention_efficacy), 100, row$intervention_efficacy)

      intervention_result <- apply_intervention(
        observed_behaviour$audit_score,
        df_rank$intervention_flag,
        base_eff_val, efficacy_pct,
        row$intervention_type,
        as.matrix(observed_parameters)
      )

      updated_behaviour <- observed_behaviour
      updated_behaviour$audit_score <- intervention_result$behaviour
      updated_parameters <- intervention_result$parameters

      n_targeted <- sum(df_rank$intervention_flag)
      ch8_debug_scenario(idx, max_runs, row, "targeted",
                         sprintf(" | %d actors targeted", n_targeted))

      # --- 3. Forward simulation (from 3_intervention_siena_run1.R) ---
      if (args$dry_run) {
        set_deterministic_seed(2022L + idx)
        stats <- setNames(rep(0, 19), c(
          "mean_value", "sd_value", "n_non_drinker", "n_1_to_4", "n_5_to_8", "n_9_to_12",
          "peak_run_index",
          paste0("peak_run_", c("mean_value","sd_value","n_non_drinker","n_1_to_4","n_5_to_8","n_9_to_12")),
          paste0("last_run_", c("mean_value","sd_value","n_non_drinker","n_1_to_4","n_5_to_8","n_9_to_12"))))
        InitSim <- NULL
      } else {
        beh_updated <- updated_behaviour$audit_score
        beh_next    <- behaviour_array[, payload_sim_idx]
        net_current <- network_array[, , payload_wave_idx]
        net_next    <- network_array[, , payload_sim_idx]

        ch8_debug_scenario(idx, max_runs, row, "siena07",
                           sprintf(" | n=%d, n3=%d", length(beh_updated), input_nthree))

        InitSim <- build_siena_forward_sim(
          updated_behaviour   = beh_updated,
          next_wave_behaviour = beh_next,
          network_current     = net_current,
          network_next        = net_next,
          actor_covariates    = actor_covariates,
          dyadic_covariates   = dyadic_covariates,
          updated_parameters  = as.matrix(updated_parameters),
          input_nthree        = input_nthree,
          cores               = cl
        )

        stats <- collect_sim_stats(InitSim)
        ch8_debug_scenario(idx, max_runs, row, "DONE",
                           sprintf(" | mean_audit=%.2f", stats["mean_value"]))
      }

      intervention_count <- if (row$intervention_type == "C") nrow(df_rank) else n_targeted
      result_row <- data.frame(
        input_index_strategy     = idx,
        as.list(stats),
        intervention_count       = intervention_count,
        intervention_wave        = intervention_wave,
        simulated_wave           = simulated_wave,
        intervention_type        = row$intervention_type,
        intervention_targeting   = row$intervention_targeting,
        intervention_efficacy    = row$intervention_efficacy,
        intervention_proportion  = row$intervention_proportion,
        stringsAsFactors = FALSE
      )

      list(status = "ok", result_row = result_row, df_rank = df_rank,
           InitSim = InitSim)

    }, error = function(e) {
      ch8_debug_scenario(idx, max_runs, row, "ERROR",
                         sprintf(" | %s", e$message))
      # Save checkpoint before dying so we can resume from here
      completed_results <- results_list[!vapply(results_list, is.null, logical(1))]
      if (length(completed_results) > 0) {
        df_results <- do.call(rbind, completed_results)
        ensure_parent_dir(results_csv_path)
        write.csv(df_results, results_csv_path, row.names = FALSE)
        if (!args$dry_run) {
          ensure_parent_dir(cache_initsim_path)
          save(list_initsim, file = cache_initsim_path)
          save(list_rank,    file = cache_rank_path)
        }
        ch8_debug("Emergency checkpoint saved: ", nrow(df_results),
                  " completed scenarios preserved. Re-run to resume from scenario ", idx)
      }
      stop(sprintf("Scenario %d/%d failed: %s\nRe-run the script to resume from this point.",
                   idx, max_runs, e$message), call. = FALSE)
    })

    # --- Process the result (error handler above already dies on failure) ---
    elapsed_s <- proc.time()["elapsed"] - t_scenario

    results_list[[idx]] <- scenario_result$result_row
    list_rank[[idx]]    <- scenario_result$df_rank
    if (!is.null(scenario_result$InitSim)) {
      list_initsim[[idx]] <- scenario_result$InitSim
    }
    completed[[idx]] <- TRUE
    scenarios_run <- scenarios_run + 1L
    cat(sprintf("  [%d/%d] OK [%.1fs]\n", idx, max_runs, elapsed_s))

    # --- Checkpoint every 10 scenarios or at the end ---
    if (scenarios_run %% 10 == 0 || idx == max_runs) {
      completed_results <- results_list[!vapply(results_list, is.null, logical(1))]
      if (length(completed_results) > 0) {
        df_results <- do.call(rbind, completed_results)
        ensure_parent_dir(results_csv_path)
        write.csv(df_results, results_csv_path, row.names = FALSE)
        if (!args$dry_run) {
          ensure_parent_dir(cache_initsim_path)
          save(list_initsim, file = cache_initsim_path)
          save(list_rank,    file = cache_rank_path)
        }
        ch8_debug("Checkpoint saved: ", nrow(df_results), " results")
      }
    }
  }

  # =========================================================================
  # Final save + summary
  # =========================================================================
  completed_results <- results_list[!vapply(results_list, is.null, logical(1))]
  if (length(completed_results) > 0) {
    df_results <- do.call(rbind, completed_results)
    write.csv(df_results, results_csv_path, row.names = FALSE)
    if (!args$dry_run) {
      save(list_initsim, file = cache_initsim_path)
      save(list_rank,    file = cache_rank_path)
    }
  } else {
    df_results <- data.frame()
  }

  entry <- list(
    timestamp = format_timestamp(),
    action = "run_1st_period",
    scenarios = nrow(df_results),
    dry_run = args$dry_run,
    resumed_from = n_already_done,
    newly_completed = scenarios_run
  )
  append_pipeline_log(logs_root, "chapter8", entry, history_key = "intervention_runs")

  ch8_debug("=== Summary ===")
  ch8_debug("Total scenarios: ", max_runs)
  ch8_debug("Previously completed: ", n_already_done)
  ch8_debug("Newly completed: ", scenarios_run)

  message(sprintf("First-period simulation complete: %d scenarios → %s",
                  nrow(df_results), results_csv_path))
}

# Guard: only execute when run as a standalone script (not when sourced)
if (identical(environment(), globalenv())) {
  run()
}
