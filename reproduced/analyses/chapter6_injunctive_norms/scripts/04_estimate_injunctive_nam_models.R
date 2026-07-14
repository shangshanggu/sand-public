#!/usr/bin/env Rscript
#
# 04_estimate_injunctive_nam_models.R
#
# Estimate NAM (Network Autocorrelation Model) regressions for Chapter 6
# injunctive norms using sna::lnam().
#
# The thesis presents three "Combined" model tables (Tables 2a, 2b,
# passing-out) each covering Time 1 / Time 2 / Time 3.
#
# Each model regresses a binary drinking outcome on:
#   - misperception_innoX_global  (global-level injunctive misperception)
#   - misperception_innoX_peer    (peer-level injunctive misperception)
#   - audit_score_previous        (AUDIT-C at previous wave)
#   - age
#   - sex                         (coded 1 = male, 0 = female)
#   - if_white                    (ethnicity covariate)
#   - friend_number               (number of nominations)
#
# The adjacency matrix W1 is built from friendid columns in the Chapter 4
# prepared data and normalised using the legacy approach:
#   diag(1/colSums) %*% adj
# matching the legacy analysis exactly (same as Chapter 5).

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
  stop("Unable to determine script path for Chapter 6 NAM estimation.")
}

script_dir <- dirname(get_script_path())
repo_root  <- normalizePath(file.path(script_dir, "..", "..", ".."),
                            winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x


# ---------------------------------------------------------------------------
# Model specifications matching thesis Chapter 6 "Combined" tables
# ---------------------------------------------------------------------------
MODEL_SPECS <- list(
  list(
    outcome_key   = "drinker",
    outcome_label = "Consumed alcohol over the past month",
    outcome_col   = "if_drinker",
    misp_global   = "misperception_inno1_global",
    misp_peer     = "misperception_inno1_peer"
  ),
  list(
    outcome_key   = "binge_drinker",
    outcome_label = "Binged drunk over the past month",
    outcome_col   = "if_bingedrinker",
    misp_global   = "misperception_inno2_global",
    misp_peer     = "misperception_inno2_peer"
  ),
  list(
    outcome_key   = "passing_out",
    outcome_label = "If passing out",
    outcome_col   = "if_passout",
    misp_global   = "misperception_inno3_global",
    misp_peer     = "misperception_inno3_peer"
  )
)

TIME_PERIODS <- c("Time 1" = "time1", "Time 2" = "time2", "Time 3" = "time3")

COVARIATE_COLS <- c("age", "sex", "if_white", "friend_number",
                     "audit_score_previous")

FRIENDID_COLS <- paste0("friendid", 1:10)

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

# Build previous-wave AUDIT-C score column
add_previous_audit <- function(current_df, prev_df) {
  prev_lookup <- stats::setNames(prev_df$audit_score,
                                  prev_df$redcap_survey_identifier)
  current_df$audit_score_previous <- unname(
    prev_lookup[as.character(current_df$redcap_survey_identifier)]
  )
  current_df
}

# Build adjacency matrix from friendid columns with legacy normalisation.
# This matches the legacy approach exactly:
#   1. Directed graph from nominations (friendid1-friendid10)
#   2. Normalise via diag(1/colSums) %*% adj
#      (divides each row i by the column-sum of column i, i.e. the in-degree
#       of node i — matching the original thesis code)
build_adjacency_matrix <- function(data) {
  ids <- as.character(data$redcap_survey_identifier)
  n <- length(ids)
  id_index <- stats::setNames(seq_len(n), ids)

  adj <- matrix(0, nrow = n, ncol = n)
  rownames(adj) <- ids
  colnames(adj) <- ids

  fid_cols <- intersect(FRIENDID_COLS, names(data))
  for (col in fid_cols) {
    nominees <- as.character(data[[col]])
    for (i in seq_len(n)) {
      nominee <- nominees[i]
      if (!is.na(nominee) && nzchar(nominee) && nominee %in% ids) {
        j <- id_index[[nominee]]
        adj[i, j] <- 1
      }
    }
  }

  # Legacy column-normalisation: diag(1/colSums) %*% adj
  col_sums <- colSums(adj)
  col_inv <- ifelse(col_sums > 0, 1 / col_sums, 0)
  adj <- diag(col_inv) %*% adj
  rownames(adj) <- ids
  colnames(adj) <- ids

  adj
}

# Prepare model data: select complete cases, build adjacency, align
prepare_model_inputs <- function(data, spec) {
  all_cols <- c("redcap_survey_identifier", spec$outcome_col,
                spec$misp_global, spec$misp_peer, COVARIATE_COLS, FRIENDID_COLS)
  keep_cols <- intersect(all_cols, names(data))
  model_cols <- c(spec$outcome_col, spec$misp_global, spec$misp_peer,
                  COVARIATE_COLS)
  missing <- setdiff(model_cols, names(data))
  if (length(missing) > 0) {
    stop(sprintf("Missing columns for model %s: %s",
                 spec$outcome_key, paste(missing, collapse = ", ")))
  }

  # LOCF further imputation: if exactly 1 misperception column is NA, impute
  # with the mean computed from the flagged subset only (matching legacy
  # dplyr pipeline: filter(further_imputation==1) %>% mutate_at(~ifelse(is.na(.),mean(.,na.rm=TRUE),.)))
  misp_cols <- c(spec$misp_global, spec$misp_peer)
  na_count <- rowSums(is.na(data[, misp_cols, drop = FALSE]))
  further_imp <- na_count == 1

  if (any(further_imp)) {
    for (mc in misp_cols) {
      col_mean <- mean(data[further_imp, mc], na.rm = TRUE)
      data[[mc]] <- ifelse(further_imp & is.na(data[[mc]]), col_mean, data[[mc]])
    }
  }

  # Complete cases on model columns
  complete_idx <- stats::complete.cases(data[, model_cols, drop = FALSE])
  data <- data[complete_idx, , drop = FALSE]

  if (nrow(data) < 10) {
    stop(sprintf("Too few complete cases (%d) for model %s",
                 nrow(data), spec$outcome_key))
  }

  # Build adjacency matrix for these participants
  W1 <- build_adjacency_matrix(data)

  # Covariate matrix (misperceptions + covariates, matching legacy order)
  x_cols <- c(COVARIATE_COLS, spec$misp_peer, spec$misp_global)
  X <- as.matrix(data[, x_cols, drop = FALSE])

  # Response vector
  y <- as.numeric(data[[spec$outcome_col]])

  list(y = y, X = X, W1 = W1, ids = data$redcap_survey_identifier,
       n = nrow(data), x_cols = x_cols)
}

fit_lnam_model <- function(inputs, spec) {
  model <- sna::lnam(y = inputs$y, x = inputs$X, W1 = inputs$W1)
  smry <- summary(model)

  # Extract beta coefficients
  beta_names <- names(smry$beta)
  if (is.null(beta_names)) beta_names <- inputs$x_cols

  # Map column names to thesis-style labels
  label_map <- c(
    "(Intercept)"          = "Intercept",
    "audit_score_previous" = "AUDIT-C score at previous wave",
    "age"                  = "Age",
    "sex"                  = "Gender",
    "if_white"             = "Ethnicity (if white)",
    "friend_number"        = "Number of nominations"
  )
  label_map[[spec$misp_global]] <- "Global-level misperception"
  label_map[[spec$misp_peer]]   <- "Peer-level misperception"

  # Beta rows
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

  # rho1 row (network autocorrelation parameter)
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
  ensure_chapter_enabled(bundle, "chapter6_injunctive_norms")

  ch4_paths <- resolve_chapter_output_paths(bundle, "chapter4_data_collection")
  ch6_paths <- resolve_chapter_output_paths(bundle, "chapter6_injunctive_norms")

  manifest_path <- file.path(ch4_paths$manifests, "prepared_data_manifest.json")
  manifest <- load_manifest(manifest_path)

  # Load all wave datasets (baseline through time3) for previous-audit lookup
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

  # Also add static covariates from baseline (matching legacy approach)
  baseline <- wave_data[["baseline"]]
  for (wk in c("time1", "time2", "time3")) {
    bl_lookup_age <- stats::setNames(as.numeric(baseline$age),
                                      baseline$redcap_survey_identifier)
    bl_lookup_sex <- stats::setNames(as.numeric(baseline$sex),
                                      baseline$redcap_survey_identifier)
    bl_lookup_white <- stats::setNames(as.numeric(baseline$if_white),
                                        baseline$redcap_survey_identifier)
    ids <- as.character(wave_data[[wk]]$redcap_survey_identifier)
    wave_data[[wk]]$age <- unname(bl_lookup_age[ids])
    wave_data[[wk]]$sex <- unname(bl_lookup_sex[ids])
    wave_data[[wk]]$if_white <- unname(bl_lookup_white[ids])
  }

  all_coefs <- list()
  all_diags <- list()
  idx <- 1

  for (spec in MODEL_SPECS) {
    for (period_label in names(TIME_PERIODS)) {
      wave_key <- TIME_PERIODS[[period_label]]
      data <- wave_data[[wave_key]]

      message(sprintf("[chapter6] Fitting lnam: %s / %s (n=%d)",
                      spec$outcome_key, period_label, nrow(data)))

      inputs <- prepare_model_inputs(data, spec)
      result <- fit_lnam_model(inputs, spec)

      coef_df <- result$coefficients
      coef_df$outcome_key   <- spec$outcome_key
      coef_df$outcome_label <- spec$outcome_label
      coef_df$time_period   <- period_label
      all_coefs[[idx]] <- coef_df

      all_diags[[idx]] <- data.frame(
        outcome_key   = spec$outcome_key,
        outcome_label = spec$outcome_label,
        time_period   = period_label,
        n_obs         = result$n_obs,
        df_total      = result$df_total,
        sigma         = result$sigma,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }

  coefs_df <- do.call(rbind, all_coefs)
  rownames(coefs_df) <- NULL
  diags_df <- do.call(rbind, all_diags)
  rownames(diags_df) <- NULL

  # Write outputs
  coefs_csv  <- file.path(ch6_paths$tables, "injunctive_nam_coefficients.csv")
  coefs_json <- file.path(ch6_paths$manifests, "injunctive_nam_coefficients.json")
  diags_csv  <- file.path(ch6_paths$tables, "injunctive_nam_diagnostics.csv")

  old_opts <- options(digits = 16)
  on.exit(options(old_opts), add = TRUE)

  write.csv(coefs_df, coefs_csv, row.names = FALSE)
  write.csv(diags_df, diags_csv, row.names = FALSE)

  payload <- list(
    generated_at = format_timestamp(),
    estimation_method = "sna::lnam",
    models = length(MODEL_SPECS) * length(TIME_PERIODS),
    coefficients = coefs_df,
    diagnostics  = diags_df
  )
  write_json(payload, coefs_json, auto_unbox = TRUE, pretty = TRUE)

  # Update run log
  logs_rel  <- get_config_value(bundle, "project", "paths", "logs_dir",
                                required = TRUE)
  logs_root <- resolve_repo_path(bundle, logs_rel, must_exist = FALSE)
  log_entry <- list(
    generated_at = format_timestamp(),
    estimation_method = "sna::lnam",
    outputs = relativise_paths(
      list(coefficients_csv = coefs_csv,
           coefficients_json = coefs_json,
           diagnostics_csv = diags_csv),
      bundle$repo_root
    )
  )
  log_path <- file.path(ch6_paths$logs, "run.json")
  append_run_log(log_path, log_entry)
  if (!is.null(logs_root) && nzchar(logs_root)) {
    append_pipeline_log(logs_root, "chapter6", log_entry)
  }

  message("[chapter6] NAM coefficient table written to ", coefs_csv)
  message("[chapter6] NAM diagnostics written to ", diags_csv)
  message("[chapter6] NAM manifest stored at ", coefs_json)
  message("[chapter6] Run log updated at ", log_path)
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error estimating Chapter 6 NAM models: ", e$message)
      quit(status = 1)
    }
  )
}
