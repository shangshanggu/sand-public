#!/usr/bin/env Rscript
#
# 02_estimate_nam_models.R
#
# Estimate NAM (Network Autocorrelation Model) regressions for Chapter 5
# descriptive norms using sna::lnam(), replicating the legacy analysis.
#
# The thesis presents four model specifications at each time point (Table
# \ref{tab:nam-results}).  This script reproduces the "Both" model (Model 4)
# which includes global-level and peer-level misperception as predictors.
#
# Outcome: audit_score (self-reported AUDIT-C)
# Covariates (in legacy order):
#   - age, sex, if_white, friend_number, audit_score_previous
#   - misperception_audit_c_peer    (peer-level descriptive misperception)
#   - misperception_audit_c_global  (global-level descriptive misperception)
#
# The adjacency matrix W1 is built from the nomination edge list in
# list_by_wave.RData and column-normalised, matching the legacy analysis.

suppressPackageStartupMessages({
  library(jsonlite)
  if (!requireNamespace("sna", quietly = TRUE)) {
    stop("The 'sna' package is required for lnam(). Install via install.packages('sna').")
  }
  if (!requireNamespace("numDeriv", quietly = TRUE)) {
    stop("The 'numDeriv' package is required by sna::lnam(). Install via install.packages('numDeriv').")
  }
})

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- grep(file_arg, cmd_args)
  if (length(matches) > 0) {
    return(normalizePath(sub(file_arg, "", cmd_args[matches[1]]),
                         winslash = "/", mustWork = TRUE))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile,
                         winslash = "/", mustWork = TRUE))
  }
  stop("Unable to determine script path for Chapter 5 NAM estimation.")
}

script_dir <- dirname(get_script_path())
repo_root  <- normalizePath(file.path(script_dir, "..", "..", ".."),
                            winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Legacy wave mapping: Time 1 → wave index 2, Time 2 → wave index 5, Time 3 → wave index 6
TIME_PERIODS <- list(
  "Time 1" = list(wave_key = "time1", lbw_index = 2),
  "Time 2" = list(wave_key = "time2", lbw_index = 5),
  "Time 3" = list(wave_key = "time3", lbw_index = 6)
)

# Covariate order matching legacy: age, sex, if_white, friend_number,
# audit_score_previous, then misperceptions (peer before global)
COVARIATE_COLS <- c("age", "sex", "if_white", "friend_number",
                     "audit_score_previous")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
load_manifest <- function(manifest_path) {
  if (!file.exists(manifest_path)) {
    stop(sprintf("Missing prepared_data_manifest.json at %s", manifest_path))
  }
  fromJSON(manifest_path, simplifyVector = FALSE)
}

find_entry <- function(manifest, wave, imputation = "LOCF",
                       typical = "mean") {
  matches <- Filter(function(e) {
    tolower(e$wave) == tolower(wave) &&
      tolower(e$imputation_method) == tolower(imputation) &&
      tolower(e$typical_definition) == tolower(typical)
  }, manifest$entries)
  if (length(matches) == 0) {
    stop(sprintf("No manifest entry for wave=%s, imputation=%s, typical=%s",
                 wave, imputation, typical))
  }
  matches[[1]]
}

load_wave_data <- function(bundle, entry_path) {
  resolved <- resolve_repo_path(bundle, entry_path, must_exist = FALSE)
  if (!file.exists(resolved)) {
    stop(sprintf("Prepared data file missing: %s", resolved))
  }
  read.csv(resolved, stringsAsFactors = FALSE)
}

load_list_by_wave <- function(bundle) {
  raw_dir <- resolve_data_dir(bundle)
  lbw_path <- file.path(raw_dir, "list_by_wave.RData")
  if (!file.exists(lbw_path)) {
    stop(sprintf("list_by_wave.RData missing at %s", lbw_path))
  }
  env <- new.env(parent = emptyenv())
  load(lbw_path, envir = env)
  if (!exists("list_by_wave", envir = env)) {
    stop("list_by_wave.RData does not contain 'list_by_wave' object.")
  }
  get("list_by_wave", envir = env)
}

add_previous_audit <- function(current_df, prev_df) {
  prev_lookup <- stats::setNames(prev_df$audit_score,
                                  prev_df$redcap_survey_identifier)
  current_df$audit_score_previous <- unname(
    prev_lookup[as.character(current_df$redcap_survey_identifier)]
  )
  current_df
}

# Build column-normalised adjacency matrix from nomination edge list.
# This matches the legacy approach exactly:
#   1. Filter edge list to participants in model data
#   2. Build directed adjacency via igraph
#   3. Column-normalise (each column sums to 1)
build_adjacency_matrix <- function(lbw_wave, participant_ids) {
  ids <- as.character(participant_ids)

  # Extract edge list: redcap_survey_identifier -> nomination
  edges <- lbw_wave[, c("redcap_survey_identifier", "nomination")]
  edges$redcap_survey_identifier <- as.character(edges$redcap_survey_identifier)
  edges$nomination <- as.character(edges$nomination)

  # Filter to non-NA nominations between participants in the model data
  edges <- edges[!is.na(edges$nomination) & nzchar(edges$nomination), ]
  edges <- edges[edges$redcap_survey_identifier %in% ids &
                   edges$nomination %in% ids, ]

  # Build adjacency matrix (n x n)
  n <- length(ids)
  adj <- matrix(0, nrow = n, ncol = n)
  rownames(adj) <- ids
  colnames(adj) <- ids

  id_index <- stats::setNames(seq_len(n), ids)
  for (k in seq_len(nrow(edges))) {
    from <- edges$redcap_survey_identifier[k]
    to   <- edges$nomination[k]
    i <- id_index[[from]]
    j <- id_index[[to]]
    adj[i, j] <- 1
  }

  # Column-normalise using the legacy approach:
  #   Diagonal(1/colSums) %*% adj
  # This divides each row i by the column-sum of column i (the in-degree of
  # node i), matching the original thesis code exactly.
  col_sums <- colSums(adj)
  col_inv <- ifelse(col_sums > 0, 1 / col_sums, 0)
  adj <- diag(col_inv) %*% adj
  rownames(adj) <- ids
  colnames(adj) <- ids

  adj
}

prepare_model_inputs <- function(data, lbw_wave) {
  # Column mapping: Chapter 4 prepared data uses misperception_audit_score_peer
  # but legacy uses misperception_audit_c_peer. Rename for consistency.
  if ("misperception_audit_score_peer" %in% names(data) &&
      !"misperception_audit_c_peer" %in% names(data)) {
    names(data)[names(data) == "misperception_audit_score_peer"] <-
      "misperception_audit_c_peer"
  }

  model_cols <- c("audit_score", "misperception_audit_c_global",
                   "misperception_audit_c_peer", COVARIATE_COLS)
  missing <- setdiff(model_cols, names(data))
  if (length(missing) > 0) {
    stop(sprintf("Missing columns: %s", paste(missing, collapse = ", ")))
  }

  # Select only model columns + identifier + friendid cols
  data <- data[, c("redcap_survey_identifier", model_cols), drop = FALSE]

  # LOCF further imputation: if exactly 1 misperception column is NA, impute
  # with the mean computed from the flagged subset only (matching legacy
  # dplyr pipeline: filter(further_imputation==1) %>% mutate_at(~ifelse(is.na(.),mean(.,na.rm=TRUE),.)))
  misp_cols <- c("misperception_audit_c_peer", "misperception_audit_c_global")
  na_count <- rowSums(is.na(data[, misp_cols, drop = FALSE]))
  further_imp <- na_count == 1

  if (any(further_imp)) {
    for (mc in misp_cols) {
      col_mean <- mean(data[further_imp, mc], na.rm = TRUE)
      data[[mc]] <- ifelse(further_imp & is.na(data[[mc]]), col_mean, data[[mc]])
    }
  }

  # Drop rows with any remaining NAs in model columns
  complete_idx <- stats::complete.cases(data[, model_cols, drop = FALSE])
  data <- data[complete_idx, , drop = FALSE]

  if (nrow(data) < 10) {
    stop(sprintf("Too few complete cases (%d) for NAM model", nrow(data)))
  }

  # Build adjacency from edge list, filtering to model participants
  W1 <- build_adjacency_matrix(lbw_wave, data$redcap_survey_identifier)

  # Covariate matrix: legacy order is covariates, then peer, then global
  x_cols <- c(COVARIATE_COLS, "misperception_audit_c_peer",
              "misperception_audit_c_global")
  X <- as.matrix(data[, x_cols, drop = FALSE])
  y <- as.numeric(data$audit_score)

  list(y = y, X = X, W1 = W1, ids = data$redcap_survey_identifier,
       n = nrow(data), x_cols = x_cols)
}

fit_lnam_model <- function(inputs) {
  model <- sna::lnam(y = inputs$y, x = inputs$X, W1 = inputs$W1)
  smry <- summary(model)

  beta_names <- names(smry$beta)
  if (is.null(beta_names)) beta_names <- c("(Intercept)", inputs$x_cols)

  label_map <- c(
    "(Intercept)"                  = "Intercept",
    "audit_score_previous"         = "AUDIT-C score at previous wave",
    "age"                          = "Age",
    "sex"                          = "Gender",
    "if_white"                     = "Ethnicity (if white)",
    "friend_number"                = "Number of nominations",
    "misperception_audit_c_global" = "Global-level misperception",
    "misperception_audit_c_peer"   = "Peer-level misperception"
  )

  rows <- list()
  for (i in seq_along(smry$beta)) {
    bn <- beta_names[i]
    est <- smry$beta[i]
    se  <- smry$beta.se[i]
    tv  <- est / se
    pv  <- 2 * stats::pt(abs(tv), smry$df.total, lower.tail = FALSE)
    rows[[i]] <- data.frame(
      term_raw   = bn,
      term_label = unname(label_map[bn] %||% bn),
      estimate   = est,
      std_error  = se,
      t_value    = tv,
      p_value    = pv,
      stringsAsFactors = FALSE
    )
  }

  # rho1 (network autocorrelation)
  rho_tv <- smry$rho1 / smry$rho1.se
  rho_pv <- 2 * stats::pt(abs(rho_tv), smry$df.total, lower.tail = FALSE)
  rows[[length(rows) + 1]] <- data.frame(
    term_raw   = "rho1",
    term_label = "Network autocorrelation (rho1)",
    estimate   = smry$rho1,
    std_error  = smry$rho1.se,
    t_value    = rho_tv,
    p_value    = rho_pv,
    stringsAsFactors = FALSE
  )

  coef_df <- do.call(rbind, rows)
  rownames(coef_df) <- NULL

  list(
    coefficients = coef_df,
    n_obs        = inputs$n,
    df_total     = smry$df.total,
    sigma        = smry$sigma
  )
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
main <- function() {
  bundle <- load_configuration()
  ensure_chapter_enabled(bundle, "chapter5_descriptive_norms")

  ch4_paths <- resolve_chapter_output_paths(bundle, "chapter4_data_collection")
  ch5_paths <- resolve_chapter_output_paths(bundle, "chapter5_descriptive_norms")

  manifest_path <- file.path(ch4_paths$manifests, "prepared_data_manifest.json")
  manifest <- load_manifest(manifest_path)

  # Load list_by_wave for adjacency matrix construction
  list_by_wave <- load_list_by_wave(bundle)

  # Load wave datasets
  wave_keys <- c("baseline", "time1", "time2", "time3")
  wave_data <- list()
  for (wk in wave_keys) {
    entry <- find_entry(manifest, wk)
    wave_data[[wk]] <- load_wave_data(bundle, entry$path)
  }

  # Build previous-audit columns
  wave_data[["time1"]] <- add_previous_audit(wave_data[["time1"]],
                                              wave_data[["baseline"]])
  wave_data[["time2"]] <- add_previous_audit(wave_data[["time2"]],
                                              wave_data[["time1"]])
  wave_data[["time3"]] <- add_previous_audit(wave_data[["time3"]],
                                              wave_data[["time2"]])

  # Static covariates from baseline
  baseline <- wave_data[["baseline"]]
  for (wk in c("time1", "time2", "time3")) {
    bl_lookup_age   <- stats::setNames(as.numeric(baseline$age),
                                        baseline$redcap_survey_identifier)
    bl_lookup_sex   <- stats::setNames(as.numeric(baseline$sex),
                                        baseline$redcap_survey_identifier)
    bl_lookup_white <- stats::setNames(as.numeric(baseline$if_white),
                                        baseline$redcap_survey_identifier)
    ids <- as.character(wave_data[[wk]]$redcap_survey_identifier)
    wave_data[[wk]]$age      <- unname(bl_lookup_age[ids])
    wave_data[[wk]]$sex      <- unname(bl_lookup_sex[ids])
    wave_data[[wk]]$if_white <- unname(bl_lookup_white[ids])
  }

  all_coefs <- list()
  all_diags <- list()
  idx <- 1

  for (period_label in names(TIME_PERIODS)) {
    tp <- TIME_PERIODS[[period_label]]
    data <- wave_data[[tp$wave_key]]
    lbw_wave <- list_by_wave[[tp$lbw_index]]

    message(sprintf("[chapter5] Fitting lnam: audit_score / %s (n=%d)",
                    period_label, nrow(data)))

    inputs <- prepare_model_inputs(data, lbw_wave)
    result <- fit_lnam_model(inputs)

    message(sprintf("[chapter5]   Complete cases used: %d", result$n_obs))

    coef_df <- result$coefficients
    coef_df$time_period <- period_label
    all_coefs[[idx]] <- coef_df

    all_diags[[idx]] <- data.frame(
      time_period = period_label,
      n_obs       = result$n_obs,
      df_total    = result$df_total,
      sigma       = result$sigma,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }

  coefs_df <- do.call(rbind, all_coefs)
  rownames(coefs_df) <- NULL
  diags_df <- do.call(rbind, all_diags)
  rownames(diags_df) <- NULL

  # Write outputs (keep backward-compatible filenames)
  summary_csv  <- file.path(ch5_paths$tables, "nam_summary.csv")
  summary_json <- file.path(ch5_paths$manifests, "nam_summary.json")
  diags_csv    <- file.path(ch5_paths$tables, "nam_diagnostics.csv")

  old_opts <- options(digits = 16)
  on.exit(options(old_opts), add = TRUE)

  write.csv(coefs_df, summary_csv, row.names = FALSE)
  write.csv(diags_df, diags_csv, row.names = FALSE)

  payload <- list(
    generated_at      = format_timestamp(),
    estimation_method = "sna::lnam",
    models            = length(TIME_PERIODS),
    coefficients      = coefs_df,
    diagnostics       = diags_df
  )
  write_json(payload, summary_json, auto_unbox = TRUE, pretty = TRUE)

  # Update run log
  logs_rel  <- get_config_value(bundle, "project", "paths", "logs_dir",
                                required = TRUE)
  logs_root <- resolve_repo_path(bundle, logs_rel, must_exist = FALSE)
  log_entry <- list(
    generated_at      = format_timestamp(),
    estimation_method = "sna::lnam",
    outputs = relativise_paths(
      list(summary_csv = summary_csv,
           summary_json = summary_json,
           diagnostics_csv = diags_csv),
      bundle$repo_root
    )
  )
  log_path <- file.path(ch5_paths$logs, "run.json")
  append_run_log(log_path, log_entry)
  if (!is.null(logs_root) && nzchar(logs_root)) {
    append_pipeline_log(logs_root, "chapter5", log_entry)
  }

  message("[chapter5] NAM coefficient table written to ", summary_csv)
  message("[chapter5] NAM diagnostics written to ", diags_csv)
  message("[chapter5] NAM manifest stored at ", summary_json)
  message("[chapter5] Run log updated at ", log_path)
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error estimating Chapter 5 NAM models: ", e$message)
      quit(status = 1)
    }
  )
}
