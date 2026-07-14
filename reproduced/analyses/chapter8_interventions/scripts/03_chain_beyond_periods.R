#!/usr/bin/env Rscript
# 03_chain_beyond_periods.R
#
# Chains the forward simulation across 2nd and 3rd periods.
# For each scenario that started at an earlier wave, takes the last (nth)
# simulation run's network + behaviour as the starting state for the next
# period, then re-runs siena07 in simOnly mode WITHOUT re-applying the
# intervention (the intervention was a one-off).
#
# Ported from z_intervention_wrapper_2beyond.R and z_intervention_wrapper_3beyond.R
#
# Outputs:
#   - reproduced/outputs/chapter8/results/df_results_2nd.csv
#   - reproduced/outputs/chapter8/results/df_results_3rd.csv
#   - reproduced/outputs/chapter8/cache/list_initsim_{2nd,3rd}.RData

suppressPackageStartupMessages({
library(yaml)
library(jsonlite)
library(dplyr)
library(igraph)
library(abind)
library(parallel)
})

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

# Source the simulation helpers from 02 into a local environment so that the
# guarded run() block (which checks `identical(environment(), globalenv())`)
# does NOT fire — the sourced code sees a child environment, not globalenv().
source(file.path(script_dir(), "02_run_interventions.R"), local = TRUE)

# ===========================================================================
# Extract simulated network as adjacency matrix from a siena07 result
# (ported from z_intervention_wrapper_2beyond.R edge-list -> adjacency logic)
# ===========================================================================
extract_sim_network <- function(InitSim, sim_index, n_actors) {
  net_sim <- InitSim$sims[[sim_index]]$Data1$SAOM_friendship[[1]]
  # net_sim is an edge list with integer actor indices
  adj <- matrix(0L, nrow = n_actors, ncol = n_actors)
  if (nrow(net_sim) > 0) {
    for (r in seq_len(nrow(net_sim))) {
      i <- net_sim[r, 1]
      j <- net_sim[r, 2]
      if (i >= 1 && i <= n_actors && j >= 1 && j <= n_actors) adj[i, j] <- 1L
    }
  }
  adj
}

extract_sim_behaviour <- function(InitSim, sim_index) {
  InitSim$sims[[sim_index]]$Data1$SAOM_behaviour[[1]]
}

# ===========================================================================
# Main
# ===========================================================================
run <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  cfg  <- yaml::read_yaml(args$config)
  int_cfg <- yaml::read_yaml(file.path(repo_root, "reproduced", "config",
                                        "scenarios", "interventions.yml"))
  logs_root <- file.path(repo_root, cfg$project$paths$logs_dir %||% "reproduced/logs")
  ch8_out   <- file.path(repo_root, cfg$chapters$chapter8_interventions$outputs_dir
                          %||% "reproduced/outputs/chapter8")

  # Wave mapping (thesis §8): survey waves -> payload indices -> next period
  waves_survey <- c(2L, 4L, 5L, 6L)
  waves_period <- c(4L, 5L, 6L, NA_integer_)
  wave_to_payload_idx <- setNames(1:4, c("2", "4", "5", "6"))

  # Load first-period results
  df_results_1st <- read.csv(file.path(ch8_out, "results", "df_results_1st.csv"),
                             stringsAsFactors = FALSE)
  df_strategy <- read.csv(file.path(ch8_out, "data", "df_intervention_strategy.csv"),
                          stringsAsFactors = FALSE)
  df_base_parameters <- readRDS(file.path(ch8_out, "data", "baseline_parameters.rds"))

  input_nthree <- as.integer(int_cfg$defaults$iterations %||% 1000)
  n_available <- detectCores()
  max_safe_cores <- max(2L, min(n_available - 4L, floor(n_available * 0.75)))
  cores <- if (!is.na(args$cores)) min(args$cores, n_available) else max_safe_cores
  message(sprintf("[cores] %d available, using %d", n_available, cores))

  # Persistent cluster — reused across all scenario simulations
  cl <- NULL
  if (!args$dry_run) {
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  # Load the network_arrays.rds payload (same source as 02)
  network_arrays_path <- file.path(repo_root,
    cfg$chapters$chapter7_saom$required_inputs[[1]] %||%
    "reproduced/outputs/chapter4/data/network_arrays.rds")
  if (!file.exists(network_arrays_path)) {
    stop(sprintf("network_arrays.rds not found at %s.\nRun Chapter 7 preprocessing first.",
                 network_arrays_path), call. = FALSE)
  }
  payload <- readRDS(network_arrays_path)

  ids               <- payload$ids
  network_array     <- payload$network_array
  behaviour_array   <- payload$behaviour_array
  actor_covariates  <- payload$actor_covariates
  dyadic_covariates <- payload$dyadic_covariates
  n_actors          <- length(ids)

  if (!args$dry_run) {
    load(file.path(ch8_out, "cache", "list_initsim_1st.RData"))
    list_initsim_1st <- list_initsim
  }

  # -----------------------------------------------------------------------
  # 2nd period: scenarios that started at wave 2 need a wave 4->5 simulation
  # -----------------------------------------------------------------------
  needs_2nd <- df_results_1st$intervention_wave == 2
  df_2nd_strategy <- df_results_1st[needs_2nd, ]
  df_2nd_strategy$intervention_index_1stref <- df_2nd_strategy$input_index_strategy
  df_2nd_strategy$intervention_wave <- 4L  # now simulating wave 4->5

  list_initsim_2nd <- vector("list", nrow(df_2nd_strategy))
  df_results_2nd   <- data.frame()

  if (nrow(df_2nd_strategy) > 0) {
    message(sprintf("Chaining %d scenarios through 2nd period (wave 4->5)", nrow(df_2nd_strategy)))

    for (idx in seq_len(nrow(df_2nd_strategy))) {
      row <- df_2nd_strategy[idx, ]
      ref_1st <- row$intervention_index_1stref
      wave_index <- which(waves_survey == 4L)
      simulated_wave <- waves_period[wave_index]  # = 5

      observed_parameters <- data.frame(
        effects = df_base_parameters$effects,
        value   = df_base_parameters[, wave_index + 1],
        stringsAsFactors = FALSE
      )

      payload_sim_idx <- wave_to_payload_idx[as.character(simulated_wave)]

      if (args$dry_run) {
        stats <- setNames(rep(0, 19), c(
          "mean_value","sd_value","n_non_drinker","n_1_to_4","n_5_to_8","n_9_to_12",
          "peak_run_index",
          paste0("peak_run_",c("mean_value","sd_value","n_non_drinker","n_1_to_4","n_5_to_8","n_9_to_12")),
          paste0("last_run_",c("mean_value","sd_value","n_non_drinker","n_1_to_4","n_5_to_8","n_9_to_12"))))
      } else {
        # Take last sim run from 1st period as starting state
        sim_beh <- extract_sim_behaviour(list_initsim_1st[[ref_1st]], input_nthree)
        sim_net <- extract_sim_network(list_initsim_1st[[ref_1st]], input_nthree, n_actors)

        # Next-wave observed behaviour from payload
        beh_next <- behaviour_array[, payload_sim_idx]

        # Next-wave observed network from payload
        net_next <- network_array[, , payload_sim_idx]

        InitSim <- build_siena_forward_sim(
          updated_behaviour   = sim_beh,
          next_wave_behaviour = beh_next,
          network_current     = sim_net,
          network_next        = net_next,
          actor_covariates    = actor_covariates,
          dyadic_covariates   = dyadic_covariates,
          updated_parameters  = as.matrix(observed_parameters),
          input_nthree        = input_nthree,
          cores               = cl
        )
        stats <- collect_sim_stats(InitSim)
        list_initsim_2nd[[idx]] <- InitSim
      }

      result_row <- data.frame(
        input_index_strategy = idx,
        as.list(stats),
        intervention_wave    = 4L,
        simulated_wave       = simulated_wave,
        intervention_type    = row$intervention_type,
        intervention_targeting  = row$intervention_targeting,
        intervention_efficacy   = row$intervention_efficacy,
        intervention_proportion = row$intervention_proportion,
        index_strategy_1stref   = ref_1st,
        stringsAsFactors = FALSE
      )
      df_results_2nd <- rbind(df_results_2nd, result_row)
    }

    ensure_parent_dir(file.path(ch8_out, "results", "df_results_2nd.csv"))
    write.csv(df_results_2nd, file.path(ch8_out, "results", "df_results_2nd.csv"), row.names = FALSE)
    if (!args$dry_run) {
      ensure_parent_dir(file.path(ch8_out, "cache", "list_initsim_2nd.RData"))
      save(list_initsim_2nd, file = file.path(ch8_out, "cache", "list_initsim_2nd.RData"))
    }
  }

  # -----------------------------------------------------------------------
  # 3rd period: scenarios starting at wave 2 or 4 need wave 5->6 simulation
  # -----------------------------------------------------------------------
  # From wave 4 (went through 1st only): use 1st period last sim
  needs_3rd_from_1st <- df_results_1st$intervention_wave == 4
  # From wave 2 (went through 1st + 2nd): use 2nd period last sim
  needs_3rd_from_2nd <- nrow(df_results_2nd) > 0

  df_3rd_parts <- list()

  # Part A: wave 4 scenarios -> 3rd period directly from 1st
  if (any(needs_3rd_from_1st)) {
    part_a <- df_results_1st[needs_3rd_from_1st, ]
    part_a$index_strategy_1stref <- part_a$input_index_strategy
    part_a$index_strategy_2ndref <- NA_integer_
    df_3rd_parts <- c(df_3rd_parts, list(part_a))
  }

  # Part B: wave 2 scenarios -> 3rd period from 2nd
  if (needs_3rd_from_2nd) {
    part_b <- df_results_2nd
    part_b$index_strategy_2ndref <- part_b$input_index_strategy
    df_3rd_parts <- c(df_3rd_parts, list(part_b))
  }

  if (length(df_3rd_parts) > 0) {
    # Harmonize columns across parts before rbinding (1st has intervention_count,
    # 2nd has index_strategy_1stref — ensure all parts share the same columns)
    all_cols <- unique(unlist(lapply(df_3rd_parts, names)))
    df_3rd_parts <- lapply(df_3rd_parts, function(df) {
      for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
      df[, all_cols, drop = FALSE]
    })
    df_3rd_strategy <- do.call(rbind, df_3rd_parts)
    df_3rd_strategy$intervention_wave <- 5L

    list_initsim_3rd <- vector("list", nrow(df_3rd_strategy))
    df_results_3rd   <- data.frame()

    message(sprintf("Chaining %d scenarios through 3rd period (wave 5->6)", nrow(df_3rd_strategy)))

    for (idx in seq_len(nrow(df_3rd_strategy))) {
      row <- df_3rd_strategy[idx, ]
      wave_index <- which(waves_survey == 5L)
      simulated_wave <- waves_period[wave_index]  # = 6

      observed_parameters <- data.frame(
        effects = df_base_parameters$effects,
        value   = df_base_parameters[, wave_index + 1],
        stringsAsFactors = FALSE
      )

      payload_sim_idx <- wave_to_payload_idx[as.character(simulated_wave)]

      if (args$dry_run) {
        stats <- setNames(rep(0, 19), c(
          "mean_value","sd_value","n_non_drinker","n_1_to_4","n_5_to_8","n_9_to_12",
          "peak_run_index",
          paste0("peak_run_",c("mean_value","sd_value","n_non_drinker","n_1_to_4","n_5_to_8","n_9_to_12")),
          paste0("last_run_",c("mean_value","sd_value","n_non_drinker","n_1_to_4","n_5_to_8","n_9_to_12"))))
      } else {
        ref_2nd <- row$index_strategy_2ndref
        ref_1st <- row$index_strategy_1stref

        if (!is.na(ref_2nd)) {
          sim_beh <- extract_sim_behaviour(list_initsim_2nd[[ref_2nd]], input_nthree)
          sim_net <- extract_sim_network(list_initsim_2nd[[ref_2nd]], input_nthree, n_actors)
        } else {
          sim_beh <- extract_sim_behaviour(list_initsim_1st[[ref_1st]], input_nthree)
          sim_net <- extract_sim_network(list_initsim_1st[[ref_1st]], input_nthree, n_actors)
        }

        beh_next <- behaviour_array[, payload_sim_idx]
        net_next <- network_array[, , payload_sim_idx]

        InitSim <- build_siena_forward_sim(
          updated_behaviour   = sim_beh,
          next_wave_behaviour = beh_next,
          network_current     = sim_net,
          network_next        = net_next,
          actor_covariates    = actor_covariates,
          dyadic_covariates   = dyadic_covariates,
          updated_parameters  = as.matrix(observed_parameters),
          input_nthree        = input_nthree,
          cores               = cl
        )
        stats <- collect_sim_stats(InitSim)
        list_initsim_3rd[[idx]] <- InitSim
      }

      result_row <- data.frame(
        input_index_strategy = idx,
        as.list(stats),
        intervention_wave    = 5L,
        simulated_wave       = simulated_wave,
        intervention_type    = row$intervention_type,
        intervention_targeting  = row$intervention_targeting,
        intervention_efficacy   = row$intervention_efficacy,
        intervention_proportion = row$intervention_proportion,
        index_strategy_1stref   = row$index_strategy_1stref,
        index_strategy_2ndref   = ifelse(is.na(row$index_strategy_2ndref), NA_integer_, row$index_strategy_2ndref),
        stringsAsFactors = FALSE
      )
      df_results_3rd <- rbind(df_results_3rd, result_row)
    }

    ensure_parent_dir(file.path(ch8_out, "results", "df_results_3rd.csv"))
    write.csv(df_results_3rd, file.path(ch8_out, "results", "df_results_3rd.csv"), row.names = FALSE)
    if (!args$dry_run) {
      ensure_parent_dir(file.path(ch8_out, "cache", "list_initsim_3rd.RData"))
      save(list_initsim_3rd, file = file.path(ch8_out, "cache", "list_initsim_3rd.RData"))
    }
  }

  entry <- list(timestamp = format_timestamp(), action = "chain_beyond_periods",
                n_2nd = nrow(df_results_2nd),
                n_3rd = if (exists("df_results_3rd")) nrow(df_results_3rd) else 0L,
                dry_run = args$dry_run)
  append_pipeline_log(logs_root, "chapter8", entry, history_key = "beyond_period_runs")

  message("Beyond-period chaining complete.")
}

run()
