#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(zoo)
  library(rlang)
  library(jsonlite)
})

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- grep(file_arg, cmd_args)
  if (length(matches) > 0) {
    return(normalizePath(sub(file_arg, "", cmd_args[matches[1]]), winslash = "/", mustWork = TRUE))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE))
  }
  stop("Unable to determine script path for configuration loader.")
}

script_dir <- dirname(get_script_path())
repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
source(file.path(repo_root, "R", "common.R"))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

load_list_by_wave <- function(list_by_wave_path) {
  if (!file.exists(list_by_wave_path)) {
    stop(
      sprintf(
        "Missing list_by_wave.RData at %s. See reproduced/docs/references/configuration_guide.md to stage raw data.",
        list_by_wave_path
      )
    )
  }
  data_env <- new.env(parent = emptyenv())
  loaded <- load(list_by_wave_path, envir = data_env)
  if (!"list_by_wave" %in% loaded) {
    stop(sprintf("Expected object 'list_by_wave' in %s.", list_by_wave_path))
  }
  get("list_by_wave", envir = data_env)
}

fun_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

fun_generate_spec_combinations <- function(spec_parameters) {
  measures <- spec_parameters$measures
  spec_parameters$measures <- NULL

  main_combinations <- expand.grid(spec_parameters)
  all_combinations <- list()

  for (i in seq_len(nrow(main_combinations))) {
    main_comb <- main_combinations[i, ]
    separate_or_together <- main_comb$separate_or_together
    current_measures <- measures[[separate_or_together]]

    for (j in seq_along(current_measures$misperceptions)) {
      current_comb_df <- data.frame(
        main_comb,
        misperception = current_measures$misperceptions[j],
        outcome = current_measures$outcomes[j],
        row.names = NULL
      )
      all_combinations <- append(all_combinations, list(current_comb_df))
    }
  }

  final_combinations <- dplyr::bind_rows(all_combinations) %>%
    mutate(
      typical_definition = if_else(reference_group == "No group", "mean", typical_definition),
      separate_or_together = if_else(reference_group == "No group", "Separate", separate_or_together),
      include_perception = if_else(reference_group == "No group", "No", include_perception)
    ) %>%
    distinct() %>%
    arrange(
      desc(imputation_method == "LOCF"),
      desc(include_perception == "No"),
      desc(model_type == "NAM"),
      desc(reference_group == "Both"),
      desc(tolower(typical_definition) == "mean"),
      desc(separate_or_together == "Separate")
    ) %>%
    mutate(index = row_number())

  final_combinations %>% filter(!is.na(typical_definition))
}

fun_preprocess_raw_data <- function(list_by_wave) {
  friends_by_wave <- list()
  friends_by_wave[[1]] <- list_by_wave[[1]] %>% select(redcap_survey_identifier)

  for (i in 2:6) {
    friends <- list_by_wave[[i]] %>%
      select(redcap_survey_identifier, nomination, which_friendid) %>%
      pivot_wider(names_from = which_friendid, values_from = nomination) %>%
      rename_with(~str_replace(., "^X", "friendid"), starts_with("X")) %>%
      {
        df <- .
        if (any(grepl("^[0-9]+$", names(df)))) {
          df <- df %>% rename_with(~paste0("friendid", .x), matches("^[0-9]+$"))
        }
        df
      } %>%
      mutate(across(starts_with("friendid"), as.character)) %>%
      select(-any_of("none"))

    friends <- friends[, seq_len(min(11, ncol(friends)))] %>%
      mutate(redcap_event_name = paste0("wave", i, "_arm_1"))

    friends_by_wave[[i]] <- list_by_wave[[1]] %>%
      select(redcap_survey_identifier) %>%
      left_join(
        friends %>% filter(redcap_event_name == paste0("wave", i, "_arm_1")),
        by = "redcap_survey_identifier"
      )
  }

  raw_variables <- c(
    "redcap_survey_identifier", "redcap_event_name", "age", "sex", "ethnicity",
    "friend_number", "audit_score", "q1", "q2", "q3", "byaacq_6", "inno1_self",
    "inno2_self", "inno3_self",
    paste0("deno1_friend_", 0:10), paste0("deno3_friend_", 0:10), paste0("deno4_friend_", 0:10),
    paste0("inno1_friend_", 0:10), paste0("inno2_friend_", 0:10), paste0("inno3_friend_", 0:10)
  )

  peer_variables <- c(
    "actual_audit_score_peer", "misperception_audit_score_peer",
    "actual_deno1_peer", "misperception_q1_peer",
    "actual_deno3_peer", "misperception_q2_peer",
    "actual_deno4_peer", "misperception_q3_peer",
    "actual_inno1_peer", "misperception_inno1_peer",
    "actual_inno2_peer", "misperception_inno2_peer",
    "actual_inno3_peer", "misperception_inno3_peer",
    "deno_audit_peer", "deno1_peer", "deno3_peer", "deno4_peer",
    "inno1_peer", "inno2_peer", "inno3_peer"
  )

  optional_vars <- c("majority_status", "residence_cluster", "number_block", "number_flat")

  select_wave_variables <- function(df) {
    df %>%
      select(
        all_of(raw_variables),
        any_of(optional_vars),
        any_of(peer_variables)
      ) %>%
      distinct()
  }

  list_norms_by_wave <- list()
  list_norms_by_wave[[1]] <- list_by_wave[[1]] %>%
    select(redcap_survey_identifier) %>%
    left_join(select_wave_variables(list_by_wave[[1]]), by = "redcap_survey_identifier") %>%
    mutate(
      byaacq_6 = if_else(q1 == 0, 0, byaacq_6),
      if_white = if_else(ethnicity == 4, 1, 0),
      if_drinker = if_else(q1 > 0, 1, 0),
      if_bingedrinker = if_else(q3 > 0, 1, 0),
      if_passout = NA_integer_
    )

  list_norms_by_wave[[2]] <- list_by_wave[[1]] %>%
    select(redcap_survey_identifier) %>%
    left_join(select_wave_variables(list_by_wave[[2]]), by = "redcap_survey_identifier") %>%
    mutate(
      byaacq_6 = if_else(q1 == 0, 0, byaacq_6),
      if_white = if_else(ethnicity == 4, 1, 0),
      if_drinker = if_else(q1 > 0, 1, 0),
      if_bingedrinker = if_else(q3 > 0, 1, 0),
      if_passout = if_else(q1 == 0, 0, byaacq_6)
    )

  list_norms_by_wave[[3]] <- list_by_wave[[1]] %>%
    select(redcap_survey_identifier) %>%
    left_join(select_wave_variables(list_by_wave[[5]]), by = "redcap_survey_identifier") %>%
    mutate(
      byaacq_6 = if_else(q1 == 0, 0, byaacq_6),
      if_white = if_else(ethnicity == 4, 1, 0),
      if_drinker = if_else(q1 > 0, 1, 0),
      if_bingedrinker = if_else(q3 > 0, 1, 0),
      if_passout = if_else(q1 == 0, 0, byaacq_6)
    )

  list_norms_by_wave[[4]] <- list_by_wave[[1]] %>%
    select(redcap_survey_identifier) %>%
    left_join(select_wave_variables(list_by_wave[[6]]), by = "redcap_survey_identifier") %>%
    mutate(
      byaacq_6 = if_else(q1 == 0, 0, byaacq_6),
      if_white = if_else(ethnicity == 4, 1, 0),
      if_drinker = if_else(q1 > 0, 1, 0),
      if_bingedrinker = if_else(q3 > 0, 1, 0),
      if_passout = if_else(q1 == 0, 0, byaacq_6)
    )

  preprocessed_data <- list()

  preprocessed_data[[1]] <- list_norms_by_wave[[1]] %>%
    mutate(
      friendid1 = NA_character_,
      friendid2 = NA_character_,
      friendid3 = NA_character_,
      friendid4 = NA_character_,
      friendid5 = NA_character_,
      friendid6 = NA_character_,
      friendid7 = NA_character_,
      friendid8 = NA_character_,
      friendid9 = NA_character_,
      friendid10 = NA_character_
    )

  preprocessed_data[[2]] <- list_norms_by_wave[[2]] %>%
    left_join(friends_by_wave[[2]], by = c("redcap_survey_identifier", "redcap_event_name"))

  preprocessed_data[[3]] <- list_norms_by_wave[[3]] %>%
    left_join(friends_by_wave[[5]], by = c("redcap_survey_identifier", "redcap_event_name"))

  preprocessed_data[[4]] <- list_norms_by_wave[[4]] %>%
    left_join(friends_by_wave[[6]], by = c("redcap_survey_identifier", "redcap_event_name"))

  preprocessed_data <- map(preprocessed_data, function(df) {
    mutate(df, across(where(is.factor), ~as.integer(as.character(.x))))
  })

  preprocessed_data
}

fun_generate_network_arrays <- function(preprocessed_data, data_dir) {
  ids <- sort(unique(preprocessed_data[[1]]$redcap_survey_identifier))
  wave_labels <- paste0("wave", seq_along(preprocessed_data))
  friend_cols <- names(preprocessed_data[[1]])[grepl("^friendid", names(preprocessed_data[[1]]))]

  network_array <- array(
    0L,
    dim = c(length(ids), length(ids), length(preprocessed_data)),
    dimnames = list(as.character(ids), as.character(ids), wave_labels)
  )

  for (w in seq_along(preprocessed_data)) {
    wave_friend_cols <- intersect(friend_cols, names(preprocessed_data[[w]]))
    if (length(wave_friend_cols) == 0) {
      next
    }

    df <- preprocessed_data[[w]] %>% distinct(redcap_survey_identifier, across(all_of(wave_friend_cols)))
    if (nrow(df) == 0) {
      next
    }

    sender_idx <- match(df$redcap_survey_identifier, ids)
    for (col in wave_friend_cols) {
      nominations <- df[[col]]
      valid <- !is.na(nominations) & nominations %in% ids
      if (any(valid, na.rm = TRUE)) {
        receiver_idx <- match(nominations[valid], ids)
        network_array[cbind(sender_idx[valid], receiver_idx, w)] <- 1L
      }
    }
  }

  behaviour_matrix <- matrix(
    NA_real_,
    nrow = length(ids),
    ncol = length(preprocessed_data),
    dimnames = list(as.character(ids), wave_labels)
  )

  for (w in seq_along(preprocessed_data)) {
    df <- preprocessed_data[[w]] %>% distinct(redcap_survey_identifier, audit_score)
    behaviour_matrix[match(df$redcap_survey_identifier, ids), w] <- df$audit_score
  }

  behaviour_lag <- behaviour_matrix
  if (ncol(behaviour_lag) > 1) {
    for (col_idx in 2:ncol(behaviour_lag)) {
      behaviour_lag[, col_idx] <- behaviour_matrix[, col_idx - 1]
    }
  }

  actor_source <- preprocessed_data[[1]]
  if (!"majority_status" %in% names(actor_source)) {
    actor_source$majority_status <- NA_integer_
  }
  if (!"residence_cluster" %in% names(actor_source)) {
    actor_source$residence_cluster <- NA_integer_
  }

  actor_covariates <- actor_source %>%
    distinct(redcap_survey_identifier, majority_status, if_white, residence_cluster, friend_number)

  actor_covariates <- actor_covariates %>%
    arrange(match(redcap_survey_identifier, ids)) %>%
    mutate(
      majority_status = coalesce(as.integer(majority_status), as.integer(if_white)),
      residence_cluster = as.integer(residence_cluster)
    ) %>%
    select(redcap_survey_identifier, majority_status, residence_cluster, friend_number)

  residence_fields <- c("number_block", "number_flat")
  missing_residence_fields <- setdiff(residence_fields, names(actor_source))
  if (length(missing_residence_fields) > 0) {
    stop(
      sprintf(
        "Cannot construct residence dyads: baseline data lack %s. Do not substitute actor-order groupings for observed or synthetic residence fields.",
        paste(missing_residence_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  residence_source <- actor_source %>%
    distinct(redcap_survey_identifier, .keep_all = TRUE) %>%
    arrange(match(redcap_survey_identifier, ids))
  blocks <- trimws(as.character(residence_source$number_block))
  flats <- trimws(as.character(residence_source$number_flat))
  has_block <- !is.na(blocks) & nzchar(blocks)
  has_flat <- !is.na(flats) & nzchar(flats)
  block_keys <- ifelse(has_block, blocks, NA_character_)
  flat_keys <- ifelse(has_block & has_flat, paste(blocks, flats, sep = "::"), NA_character_)

  flatmates <- outer(
    flat_keys,
    flat_keys,
    function(x, y) as.integer(!is.na(x) & !is.na(y) & nzchar(x) & nzchar(y) & x == y)
  )
  blockmates <- outer(
    block_keys,
    block_keys,
    function(x, y) as.integer(!is.na(x) & !is.na(y) & nzchar(x) & nzchar(y) & x == y)
  )
  diag(flatmates) <- 0L
  diag(blockmates) <- 0L
  rownames(flatmates) <- colnames(flatmates) <- as.character(ids)
  rownames(blockmates) <- colnames(blockmates) <- as.character(ids)

  dyadic_nonzero_counts <- c(
    flatmates = sum(flatmates != 0, na.rm = TRUE),
    blockmates = sum(blockmates != 0, na.rm = TRUE)
  )
  if (any(dyadic_nonzero_counts == 0)) {
    stop(
      sprintf(
        "Residence dyads are degenerate (flatmates=%d, blockmates=%d).",
        dyadic_nonzero_counts[["flatmates"]],
        dyadic_nonzero_counts[["blockmates"]]
      ),
      call. = FALSE
    )
  }

  payload <- list(
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    metadata = list(
      residence_source_fields = residence_fields,
      dyadic_nonzero_counts = as.list(dyadic_nonzero_counts)
    ),
    ids = ids,
    waves = wave_labels,
    network_array = network_array,
    behaviour_array = behaviour_matrix,
    behaviour_lag = behaviour_lag,
    actor_covariates = actor_covariates,
    dyadic_covariates = list(
      flatmates = flatmates,
      blockmates = blockmates
    )
  )

  network_path <- file.path(data_dir, "network_arrays.rds")
  saveRDS(payload, network_path)
  message("[chapter4] network_arrays.rds created at ", network_path)
  return(invisible(payload$metadata))
}

fun_perform_locf_imputation <- function(processed_data) {
  vars_to_impute <- c(
    "age", "sex", "ethnicity", "audit_score", "q1", "q2", "q3", "byaacq_6",
    paste0("deno1_friend_", 0), paste0("deno3_friend_", 0), paste0("deno4_friend_", 0),
    paste0("inno1_friend_", 0), paste0("inno2_friend_", 0), paste0("inno3_friend_", 0),
    "inno1_self", "inno2_self", "inno3_self",
    "if_white", "if_drinker", "if_bingedrinker", "if_passout"
  )

  imputed_data <- processed_data
  participant_ids <- unique(processed_data[[1]]$redcap_survey_identifier)

  for (id in participant_ids) {
    participant_data <- tibble(wave = seq_along(processed_data))

    for (wave in seq_along(processed_data)) {
      wave_data <- processed_data[[wave]] %>% filter(redcap_survey_identifier == id)
      if (nrow(wave_data) > 0) {
        participant_data[wave, names(wave_data)] <- wave_data
      }
    }

    for (var in vars_to_impute) {
      if (var %in% names(participant_data)) {
        participant_data[[var]] <- zoo::na.locf(participant_data[[var]], na.rm = FALSE)
      }
    }

    for (wave in seq_along(processed_data)) {
      imputed_cols <- intersect(names(participant_data), names(imputed_data[[wave]]))
      imputed_data[[wave]][imputed_data[[wave]]$redcap_survey_identifier == id, imputed_cols] <-
        participant_data[wave, imputed_cols, drop = FALSE]
    }
  }

  imputed_data
}

fun_calculate_perception_and_misperception <- function(processed_data) {
  prepared_data <- processed_data

  for (w in 2:4) {
    for (i in 0:10) {
      new_col_name <- paste0("deno_audit_", i)
      deno1_col <- sym(paste0("deno1_friend_", i))
      deno3_col <- sym(paste0("deno3_friend_", i))
      deno4_col <- sym(paste0("deno4_friend_", i))

      prepared_data[[w]] <- prepared_data[[w]] %>%
        mutate(!!new_col_name := !!deno1_col + !!deno3_col + !!deno4_col)
    }

    typical_y_global_mean <- prepared_data[[w]] %>%
      summarise(
        typical_audit_c_global = mean(audit_score, na.rm = TRUE),
        typical_q1_global = mean(q1, na.rm = TRUE),
        typical_q2_global = mean(q2, na.rm = TRUE),
        typical_q3_global = mean(q3, na.rm = TRUE),
        typical_inno1_global = mean(inno1_self, na.rm = TRUE),
        typical_inno2_global = mean(inno2_self, na.rm = TRUE),
        typical_inno3_global = mean(inno3_self, na.rm = TRUE)
      )
    colnames(typical_y_global_mean) <- paste0(colnames(typical_y_global_mean), "_mean")

    typical_y_global_median <- prepared_data[[w]] %>%
      summarise(
        typical_audit_c_global = median(audit_score, na.rm = TRUE),
        typical_q1_global = median(q1, na.rm = TRUE),
        typical_q2_global = median(q2, na.rm = TRUE),
        typical_q3_global = median(q3, na.rm = TRUE),
        typical_inno1_global = median(inno1_self, na.rm = TRUE),
        typical_inno2_global = median(inno2_self, na.rm = TRUE),
        typical_inno3_global = median(inno3_self, na.rm = TRUE)
      )
    colnames(typical_y_global_median) <- paste0(colnames(typical_y_global_median), "_median")

    typical_y_global_mode <- prepared_data[[w]] %>%
      summarise(
        typical_audit_c_global = fun_mode(audit_score),
        typical_q1_global = fun_mode(q1),
        typical_q2_global = fun_mode(q2),
        typical_q3_global = fun_mode(q3),
        typical_inno1_global = fun_mode(inno1_self),
        typical_inno2_global = fun_mode(inno2_self),
        typical_inno3_global = fun_mode(inno3_self)
      )
    colnames(typical_y_global_mode) <- paste0(colnames(typical_y_global_mode), "_mode")

    reference_y_global <- typical_y_global_mean %>%
      bind_cols(typical_y_global_median) %>%
      bind_cols(typical_y_global_mode)

    vector_y <- c("audit_score", "q1", "q2", "q3", "inno1_self", "inno2_self", "inno3_self")
    vector_perception <- c("deno_audit_", "deno1_friend_", "deno3_friend_", "deno4_friend_", "inno1_friend_", "inno2_friend_", "inno3_friend_")
    vector_actual <- c("actual_audit_score", "actual_deno1", "actual_deno3", "actual_deno4", "actual_inno1", "actual_inno2", "actual_inno3")
    vector_misperception <- c("misperception_audit_score", "misperception_deno1", "misperception_deno3", "misperception_deno4", "misperception_inno1", "misperception_inno2", "misperception_inno3")

    actual_y_x <- list()
    misperception_y_x <- list()

    for (y_index in seq_along(vector_y)) {
      y <- vector_y[y_index]
      true_value_y_x <- matrix(NA_real_, nrow = nrow(prepared_data[[w]]), ncol = 10)
      colnames(true_value_y_x) <- paste0(vector_actual[y_index], "_", 1:10)

      misp_value_y_x <- matrix(NA_real_, nrow = nrow(prepared_data[[w]]), ncol = 10)
      colnames(misp_value_y_x) <- paste0(vector_misperception[y_index], "_", 1:10)

      for (x in 1:10) {
        friendidx <- prepared_data[[w]][[paste0("friendid", x)]]
        for (f in seq_along(friendidx)) {
          row_idx <- which(prepared_data[[w]]$redcap_survey_identifier == friendidx[f])
          col_idx_y <- which(colnames(prepared_data[[w]]) == y)
          if (length(row_idx) > 0 && length(col_idx_y) > 0) {
            true_value_y_x[f, x] <- prepared_data[[w]][row_idx, col_idx_y]
          }

          col_idx_perc <- which(colnames(prepared_data[[w]]) == paste0(vector_perception[y_index], x))
          if (length(col_idx_perc) > 0) {
            misp_value_y_x[f, x] <- prepared_data[[w]][f, col_idx_perc] - true_value_y_x[f, x]
          }
        }
      }

      actual_y_x[[y]] <- true_value_y_x
      misperception_y_x[[y]] <- misp_value_y_x
    }

    for (y in vector_y) {
      prepared_data[[w]] <- cbind(prepared_data[[w]], actual_y_x[[y]])
      prepared_data[[w]] <- cbind(prepared_data[[w]], misperception_y_x[[y]])
    }

    compute_peer_mean <- function(df, prefix) {
      cols <- paste0(prefix, "_", 1:10)
      cols <- intersect(cols, names(df))
      if (!length(cols)) {
        return(rep(NA_real_, nrow(df)))
      }
      out <- rowMeans(as.matrix(df[, cols, drop = FALSE]), na.rm = TRUE)
      # Legacy behaviour: participants with zero friends (friend_number == 0,
      # all 10 nomination slots empty) get peer value = 0, not NaN.
      # Participants who nominated friends but whose friends all have missing
      # data keep NaN (treated as NA downstream), matching the thesis sample
      # sizes (218 / 213 / 215 for Time 1 / 2 / 3).
      zero_friends <- !is.na(df$friend_number) & df$friend_number == 0
      out[is.nan(out) & zero_friends] <- 0
      out
    }

    peer_sources <- list(
      actual_audit_score_peer = "actual_audit_score",
      actual_deno1_peer = "actual_deno1",
      actual_deno3_peer = "actual_deno3",
      actual_deno4_peer = "actual_deno4",
      actual_inno1_peer = "actual_inno1",
      actual_inno2_peer = "actual_inno2",
      actual_inno3_peer = "actual_inno3",
      misperception_audit_score_peer = "misperception_audit_score",
      misperception_q1_peer = "misperception_deno1",
      misperception_q2_peer = "misperception_deno3",
      misperception_q3_peer = "misperception_deno4",
      misperception_inno1_peer = "misperception_inno1",
      misperception_inno2_peer = "misperception_inno2",
      misperception_inno3_peer = "misperception_inno3"
    )

    for (peer_col in names(peer_sources)) {
      if (!peer_col %in% names(prepared_data[[w]])) {
        prepared_data[[w]][[peer_col]] <- compute_peer_mean(prepared_data[[w]], peer_sources[[peer_col]])
      }
    }

    derived_peer <- list(
      deno_audit_peer = c("actual_audit_score_peer", "misperception_audit_score_peer"),
      deno1_peer = c("actual_deno1_peer", "misperception_q1_peer"),
      deno3_peer = c("actual_deno3_peer", "misperception_q2_peer"),
      deno4_peer = c("actual_deno4_peer", "misperception_q3_peer"),
      inno1_peer = c("actual_inno1_peer", "misperception_inno1_peer"),
      inno2_peer = c("actual_inno2_peer", "misperception_inno2_peer"),
      inno3_peer = c("actual_inno3_peer", "misperception_inno3_peer")
    )

    for (derived_col in names(derived_peer)) {
      if (!derived_col %in% names(prepared_data[[w]])) {
        src <- derived_peer[[derived_col]]
        prepared_data[[w]][[derived_col]] <- prepared_data[[w]][[src[[1]]]] + prepared_data[[w]][[src[[2]]]]
      }
    }

    peer_calculations <- prepared_data[[w]] %>%
      mutate(
        deno_audit_peer_cal = actual_audit_score_peer + misperception_audit_score_peer,
        deno1_peer_cal = actual_deno1_peer + misperception_q1_peer,
        deno3_peer_cal = actual_deno3_peer + misperception_q2_peer,
        deno4_peer_cal = actual_deno4_peer + misperception_q3_peer,
        inno1_peer_cal = actual_inno1_peer + misperception_inno1_peer,
        inno2_peer_cal = actual_inno2_peer + misperception_inno2_peer,
        inno3_peer_cal = actual_inno3_peer + misperception_inno3_peer
      ) %>%
      mutate(
        check_deno_audit_peer = if_else(deno_audit_peer_cal == deno_audit_peer, 1L, 0L),
        check_deno1_peer = if_else(deno1_peer_cal == deno1_peer, 1L, 0L),
        check_deno3_peer = if_else(deno3_peer_cal == deno3_peer, 1L, 0L),
        check_deno4_peer = if_else(deno4_peer_cal == deno4_peer, 1L, 0L),
        check_inno1_peer = if_else(inno1_peer_cal == inno1_peer, 1L, 0L),
        check_inno2_peer = if_else(inno2_peer_cal == inno2_peer, 1L, 0L),
        check_inno3_peer = if_else(inno3_peer_cal == inno3_peer, 1L, 0L)
      )

    prepared_data[[w]] <- peer_calculations %>%
      select(-ends_with("_cal")) %>%
      bind_cols(reference_y_global)
  }

  prepared_data
}

fun_prepare_data <- function(processed_data, spec) {
  missing_2200314 <- processed_data[[1]] %>%
    filter(redcap_survey_identifier == 2200314)

  if (nrow(missing_2200314) > 0) {
    missing_2200314_impute <- processed_data[[1]] %>%
      filter(age == missing_2200314$age, sex == missing_2200314$sex, ethnicity == missing_2200314$ethnicity) %>%
      summarise(
        mean_audit_score = mean(audit_score, na.rm = TRUE),
        mean_q1 = mean(q1, na.rm = TRUE),
        mean_q2 = mean(q2, na.rm = TRUE),
        mean_q3 = mean(q3, na.rm = TRUE)
      )

    processed_data[[1]] <- processed_data[[1]] %>%
      mutate(
        q3 = if_else(redcap_survey_identifier == 2200314, as.integer(missing_2200314_impute$mean_q3), q3),
        audit_score = if_else(redcap_survey_identifier == 2200314, q1 + q2 + q3, audit_score)
      )
  }

  midprepared_data <- if (spec$imputation_method == "LOCF") {
    fun_perform_locf_imputation(processed_data)
  } else {
    processed_data
  }

  prepared_data <- fun_calculate_perception_and_misperception(midprepared_data)

  calculate_misperception <- function(deno, typical_mean, typical_median, typical_mode, definition) {
    if (definition == "mean") {
      deno - typical_mean
    } else if (definition == "median") {
      deno - typical_median
    } else if (definition == "mode") {
      deno - typical_mode
    } else {
      NA_real_
    }
  }

  for (w in 2:4) {
    prepared_data[[w]] <- prepared_data[[w]] %>%
      mutate(
        misperception_audit_c_global = calculate_misperception(deno_audit_0, typical_audit_c_global_mean, typical_audit_c_global_median, typical_audit_c_global_mode, spec$typical_definition),
        misperception_q1_global = calculate_misperception(deno1_friend_0, typical_q1_global_mean, typical_q1_global_median, typical_q1_global_mode, spec$typical_definition),
        misperception_q2_global = calculate_misperception(deno3_friend_0, typical_q2_global_mean, typical_q2_global_median, typical_q2_global_mode, spec$typical_definition),
        misperception_q3_global = calculate_misperception(deno4_friend_0, typical_q3_global_mean, typical_q3_global_median, typical_q3_global_mode, spec$typical_definition),
        misperception_inno1_global = calculate_misperception(inno1_friend_0, typical_inno1_global_mean, typical_inno1_global_median, typical_inno1_global_mode, spec$typical_definition),
        misperception_inno2_global = calculate_misperception(inno2_friend_0, typical_inno2_global_mean, typical_inno2_global_median, typical_inno2_global_mode, spec$typical_definition),
        misperception_inno3_global = calculate_misperception(inno3_friend_0, typical_inno3_global_mean, typical_inno3_global_median, typical_inno3_global_mode, spec$typical_definition)
      )
  }

  # Baseline (wave 1) has no misperception signal but needs the columns present for downstream checks.
  prepared_data[[1]] <- prepared_data[[1]] %>%
    mutate(
      misperception_audit_c_global = NA_real_,
      misperception_q1_global = NA_real_,
      misperception_q2_global = NA_real_,
      misperception_q3_global = NA_real_,
      misperception_inno1_global = NA_real_,
      misperception_inno2_global = NA_real_,
      misperception_inno3_global = NA_real_
    )

  prepared_data
}

write_prepared_data <- function(prepared_data, spec, tables_dir, repo_root) {
  spec_prefix <- paste0(spec$imputation_method, "_", spec$typical_definition)
  manifest_entries <- vector("list", length(prepared_data))
  timestamp <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  for (w in seq_along(prepared_data)) {
    wave_label <- if (w == 1) "baseline" else paste0("time", w - 1)
    file_name <- sprintf("prepared_data_%s_%s.csv", spec_prefix, wave_label)
    full_path <- file.path(tables_dir, file_name)
    readr::write_csv(prepared_data[[w]], full_path)

    file_details <- file.info(full_path)
    size_value <- as.numeric(file_details$size)
    if (length(size_value) == 0 || is.na(size_value)) {
      size_value <- NA_real_
    }

    manifest_entries[[w]] <- list(
      path = relative_repo_path(full_path, repo_root),
      imputation_method = spec$imputation_method,
      typical_definition = spec$typical_definition,
      wave = wave_label,
      rows = nrow(prepared_data[[w]]),
      columns = ncol(prepared_data[[w]]),
      size_bytes = size_value,
      generated_at = timestamp
    )
  }

  manifest_entries
}

fun_run_and_save_all_combinations <- function(preprocessed_data, data_spec, tables_dir, repo_root) {
  data_list <- list()
  manifest_entries <- list()

  for (i in seq_len(nrow(data_spec))) {
    processed_data <- purrr::map(preprocessed_data, ~ .x)
    spec <- data_spec[i, ]
    prepared_data <- fun_prepare_data(processed_data, spec)

    prepared_data <- map(prepared_data, function(df) df[, !duplicated(colnames(df))])

    data_list[[i]] <- prepared_data
    names(data_list)[[i]] <- paste0("prepared_data_", spec$imputation_method, "_", spec$typical_definition)

    manifest_entries <- c(
      manifest_entries,
      write_prepared_data(prepared_data, spec, tables_dir, repo_root)
    )
  }

  list(data_list = data_list, manifest = manifest_entries)
}

generate_participant_coverage_report <- function(preprocessed_data, qa_expectations, output_path, repo_root, config_path) {
  if (length(preprocessed_data) == 0) {
    warning("No preprocessed data available for QA report.")
    return(NULL)
  }

  wave_ids <- seq_along(preprocessed_data)
  coverage <- tibble(
    wave_index = wave_ids,
    wave_label = if_else(wave_ids == 1, "baseline", paste0("time", wave_ids - 1)),
    participants = purrr::map_int(preprocessed_data, ~dplyr::n_distinct(.x$redcap_survey_identifier))
  )

  baseline <- suppressWarnings(max(coverage$participants, na.rm = TRUE))
  if (!is.finite(baseline)) {
    baseline <- NA_real_
  }

  min_required <- qa_expectations$min_participants_per_wave %||% NA_real_
  tol_required <- qa_expectations$coverage_tolerance %||% NA_real_

  coverage <- coverage %>%
    mutate(
      relative_drop = if (!is.na(baseline) && baseline > 0) 1 - (participants / baseline) else NA_real_,
      meets_minimum = if (!is.na(min_required)) participants >= min_required else NA,
      within_tolerance = if (!is.na(tol_required)) ifelse(is.na(relative_drop), NA, relative_drop <= tol_required) else NA
    )

  overall_checks <- coverage %>%
    mutate(check = ifelse(is.na(meets_minimum) | is.na(within_tolerance), TRUE, meets_minimum & within_tolerance))
  overall_pass <- all(overall_checks$check, na.rm = TRUE)

  waves <- purrr::pmap(
    list(
      wave_index = coverage$wave_index,
      wave_label = coverage$wave_label,
      participants = coverage$participants,
      relative_drop = coverage$relative_drop,
      meets_minimum = coverage$meets_minimum,
      within_tolerance = coverage$within_tolerance
    ),
    function(wave_index, wave_label, participants, relative_drop, meets_minimum, within_tolerance) {
      list(
        wave_index = wave_index,
        wave_label = wave_label,
        participants = as.integer(participants),
        relative_drop = if (is.na(relative_drop)) NULL else round(relative_drop, 4),
        meets_minimum = if (is.na(meets_minimum)) NULL else isTRUE(meets_minimum),
        within_tolerance = if (is.na(within_tolerance)) NULL else isTRUE(within_tolerance)
      )
    }
  )

  alerts <- purrr::compact(purrr::map(waves, function(entry) {
    if (!is.null(entry$meets_minimum) && !entry$meets_minimum) {
      return(sprintf("Wave %s participant count (%s) fell below minimum threshold.", entry$wave_label, entry$participants))
    }
    if (!is.null(entry$within_tolerance) && !entry$within_tolerance) {
      drop_value <- entry$relative_drop %||% NA_real_
      return(sprintf("Wave %s exceeded allowable coverage drop (%.4f).", entry$wave_label, drop_value))
    }
    NULL
  }))

  payload <- list(
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    config = relative_repo_path(config_path, repo_root),
    expectations = list(
      min_participants_per_wave = if (is.na(min_required)) NULL else min_required,
      coverage_tolerance = if (is.na(tol_required)) NULL else tol_required
    ),
    baseline_participants = if (is.na(baseline)) NULL else as.integer(baseline),
    overall_pass = overall_pass,
    waves = waves
  )

  if (length(alerts) > 0) {
    payload$alerts <- alerts
  }

  jsonlite::write_json(payload, output_path, auto_unbox = TRUE, pretty = TRUE)
  payload
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  config_path <- if (length(args) >= 1) args[[1]] else "reproduced/config/thesis.yml"

  cfg <- load_configuration(config_path)
  ensure_chapter_enabled(cfg, "chapter4_data_collection")
  config <- cfg$config
  repo_root <- cfg$repo_root

  output_paths <- resolve_chapter_output_paths(cfg, "chapter4_data_collection")
  outputs_dir <- output_paths$base
  tables_dir <- output_paths$tables
  manifests_dir <- output_paths$manifests
  logs_dir <- output_paths$logs
  data_dir <- output_paths$data

  raw_data_dir <- resolve_data_dir(cfg)
  list_by_wave_path <- file.path(raw_data_dir, "list_by_wave.RData")

  list_by_wave <- load_list_by_wave(list_by_wave_path)

  spec_parameters <- get_config_value(
    cfg,
    "chapters.chapter4_data_collection.spec_parameters",
    required = TRUE
  )

  qa_expectations <- get_config_value(
    cfg,
    "chapters.chapter4_data_collection.qa_expectations",
    required = FALSE
  )

  final_combinations <- fun_generate_spec_combinations(spec_parameters)
  combinations_path <- file.path(tables_dir, "chapter4_specifications.csv")
  readr::write_csv(final_combinations, combinations_path)

  data_spec <- final_combinations %>% select(imputation_method, typical_definition) %>% distinct()

  preprocessed_data <- fun_preprocess_raw_data(list_by_wave)
  network_array_metadata <- fun_generate_network_arrays(preprocessed_data, data_dir)

  qa_report_path <- NULL
  if (!is.null(qa_expectations)) {
    qa_report_path <- file.path(logs_dir, "chapter4_qa_report.json")
    generate_participant_coverage_report(
      preprocessed_data,
      qa_expectations,
      qa_report_path,
      repo_root,
      cfg$config_path
    )
  }

  run_results <- fun_run_and_save_all_combinations(preprocessed_data, data_spec, tables_dir, repo_root)
  data_list <- run_results$data_list
  manifest_entries <- run_results$manifest

  save(data_list, file = file.path(data_dir, "prepared_data_sets.RData"))

  manifest_payload <- list(
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    config = relative_repo_path(cfg$config_path, repo_root),
    specification_csv = relative_repo_path(combinations_path, repo_root),
    network_arrays = network_array_metadata,
    entries = manifest_entries
  )

  if (!is.null(qa_report_path)) {
    manifest_payload$qa_report <- relative_repo_path(qa_report_path, repo_root)
  }

  manifest_path <- file.path(manifests_dir, "prepared_data_manifest.json")
  jsonlite::write_json(manifest_payload, manifest_path, auto_unbox = TRUE, pretty = TRUE)

  message("Chapter 4 data preparation completed. Outputs stored in ", outputs_dir)
}

if (identical(environment(), globalenv())) {
  main()
}
