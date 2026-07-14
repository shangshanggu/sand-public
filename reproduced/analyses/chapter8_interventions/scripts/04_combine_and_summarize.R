#!/usr/bin/env Rscript
# 04_combine_and_summarize.R
#
# Joins within-period (1st), middle (2nd), and final (3rd) results into a
# unified strategy history.  Computes percentage reductions relative to the
# 0%-coverage control condition for each intervention type × wave × efficacy.
#
# Ported from zz_combinedAnalysis.R
#
# Outputs:
#   - reproduced/outputs/chapter8/results/combined_strategy_history.csv
#   - reproduced/outputs/chapter8/results/df_results_within.csv
#   - reproduced/outputs/chapter8/results/df_results_middle.csv
#   - reproduced/outputs/chapter8/results/df_results_final.csv
#   - reproduced/outputs/chapter8/summaries/intervention_summary.json

suppressPackageStartupMessages({
library(yaml)
library(jsonlite)
library(dplyr)
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

# ---------------------------------------------------------------------------
# Outcome columns tracked across periods
# ---------------------------------------------------------------------------
outcome_cols <- c(
  "mean_value", "sd_value", "n_non_drinker", "n_1_to_4", "n_5_to_8", "n_9_to_12",
  "peak_run_index", "peak_run_mean_value", "peak_run_sd_value",
  "peak_run_n_non_drinker", "peak_run_n_1_to_4", "peak_run_n_5_to_8", "peak_run_n_9_to_12",
  "last_run_mean_value", "last_run_sd_value",
  "last_run_n_non_drinker", "last_run_n_1_to_4", "last_run_n_5_to_8", "last_run_n_9_to_12"
)

# ---------------------------------------------------------------------------
# Compute percentage reduction vs 0%-coverage control
# ---------------------------------------------------------------------------
compute_reductions <- function(df, outcome_col = "mean_value") {
  # Control = rows where proportion == 0
  df_control <- df %>%
    dplyr::filter(intervention_proportion == 0) %>%
    dplyr::select(intervention_wave, intervention_type, intervention_targeting,
                  intervention_efficacy, control_value = !!sym(outcome_col)) %>%
    dplyr::distinct()

  df %>%
    dplyr::left_join(df_control,
      by = c("intervention_wave", "intervention_type",
             "intervention_targeting", "intervention_efficacy")) %>%
    dplyr::mutate(
      outcome_reduction    = control_value - !!sym(outcome_col),
      percentage_reduction = dplyr::if_else(
        control_value != 0,
        outcome_reduction / control_value * 100,
        0
      )
    )
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
run <- function() {
  cfg <- yaml::read_yaml(file.path(repo_root, "reproduced", "config", "thesis.yml"))
  logs_root <- file.path(repo_root, cfg$project$paths$logs_dir %||% "reproduced/logs")
  ch8_out   <- file.path(repo_root, cfg$chapters$chapter8_interventions$outputs_dir
                          %||% "reproduced/outputs/chapter8")

  results_dir   <- file.path(ch8_out, "results")
  summaries_dir <- file.path(ch8_out, "summaries")
  ensure_parent_dir(file.path(summaries_dir, "x"))

  # Load period results
  df_1st <- read.csv(file.path(results_dir, "df_results_1st.csv"), stringsAsFactors = FALSE)

  has_2nd <- file.exists(file.path(results_dir, "df_results_2nd.csv"))
  has_3rd <- file.exists(file.path(results_dir, "df_results_3rd.csv"))
  df_2nd <- if (has_2nd) read.csv(file.path(results_dir, "df_results_2nd.csv"), stringsAsFactors = FALSE) else data.frame()
  df_3rd <- if (has_3rd) read.csv(file.path(results_dir, "df_results_3rd.csv"), stringsAsFactors = FALSE) else data.frame()

  # Strategy metadata columns
  strategy_cols <- c("intervention_type", "intervention_wave", "intervention_targeting",
                     "intervention_proportion", "intervention_efficacy")

  # --- Build within / middle / final views ---
  # "within"  = 1st period result for every scenario
  # "middle"  = 2nd period result where available, else 1st
  # "final"   = 3rd period result where available, else fallback to 1st

  df_within <- df_1st

  # Middle: for wave-2 scenarios, use 2nd period; otherwise use 1st
  if (nrow(df_2nd) > 0) {
    df_middle <- df_1st
    # Match 2nd-period rows back to 1st-period by index_strategy_1stref
    for (i in seq_len(nrow(df_2nd))) {
      ref <- df_2nd$index_strategy_1stref[i]
      match_row <- which(df_middle$input_index_strategy == ref)
      if (length(match_row) == 1) {
        for (col in intersect(outcome_cols, names(df_2nd))) {
          df_middle[match_row, col] <- df_2nd[i, col]
        }
        df_middle$simulated_wave[match_row] <- df_2nd$simulated_wave[i]
      }
    }
  } else {
    df_middle <- df_within
  }

  # Final: for scenarios with 3rd period, use that; else use 1st
  if (nrow(df_3rd) > 0) {
    df_final <- df_1st
    for (i in seq_len(nrow(df_3rd))) {
      ref <- df_3rd$index_strategy_1stref[i]
      match_row <- which(df_final$input_index_strategy == ref)
      if (length(match_row) == 1) {
        for (col in intersect(outcome_cols, names(df_3rd))) {
          df_final[match_row, col] <- df_3rd[i, col]
        }
        df_final$simulated_wave[match_row] <- df_3rd$simulated_wave[i]
      }
    }
  } else {
    df_final <- df_within
  }

  # Add composite outcome
  df_within$n_5_to_12 <- df_within$n_5_to_8 + df_within$n_9_to_12
  df_middle$n_5_to_12 <- df_middle$n_5_to_8 + df_middle$n_9_to_12
  df_final$n_5_to_12  <- df_final$n_5_to_8  + df_final$n_9_to_12

  # Standardise column names for the period label
  # (original code uses intervention_wave_1st etc. — we keep it simpler)
  write.csv(df_within, file.path(results_dir, "df_results_within.csv"), row.names = FALSE)
  write.csv(df_middle, file.path(results_dir, "df_results_middle.csv"), row.names = FALSE)
  write.csv(df_final,  file.path(results_dir, "df_results_final.csv"),  row.names = FALSE)

  # --- Compute reductions ---
  df_within_red <- compute_reductions(df_within, "mean_value")
  df_middle_red <- compute_reductions(df_middle, "mean_value")
  df_final_red  <- compute_reductions(df_final,  "mean_value")

  write.csv(df_within_red, file.path(results_dir, "df_analysis_within.csv"), row.names = FALSE)
  write.csv(df_middle_red, file.path(results_dir, "df_analysis_middle.csv"), row.names = FALSE)
  write.csv(df_final_red,  file.path(results_dir, "df_analysis_final.csv"),  row.names = FALSE)

  # --- Summary ---
  summary_payload <- list(
    timestamp    = format_timestamp(),
    n_1st        = nrow(df_1st),
    n_2nd        = nrow(df_2nd),
    n_3rd        = nrow(df_3rd),
    n_within     = nrow(df_within),
    n_middle     = nrow(df_middle),
    n_final      = nrow(df_final),
    outcome_cols = outcome_cols
  )
  jsonlite::write_json(summary_payload,
    file.path(summaries_dir, "intervention_summary.json"),
    auto_unbox = TRUE, pretty = TRUE)

  entry <- list(timestamp = summary_payload$timestamp, action = "combine_and_summarize",
                n_within = nrow(df_within), n_final = nrow(df_final))
  append_pipeline_log(logs_root, "chapter8", entry, history_key = "summaries")

  message(sprintf("Combined results: within=%d, middle=%d, final=%d → %s",
                  nrow(df_within), nrow(df_middle), nrow(df_final), results_dir))
}

run()
