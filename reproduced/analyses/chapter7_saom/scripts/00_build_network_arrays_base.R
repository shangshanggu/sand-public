#!/usr/bin/env Rscript

get_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) == 0) {
    return(normalizePath("."))
  }
  normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)
}

ensure_runtime_packages <- function(packages) {
  packages <- unique(packages[!is.na(packages) & nzchar(packages)])
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) {
    return(invisible(TRUE))
  }

  stop(
    sprintf(
      "Missing required R packages: %s. Install them with `make env-renv` or run `R -e 'install.packages(c(%s))'` before regenerating Chapter 7 inputs.",
      paste(missing, collapse = ", "),
      paste(sprintf("'%s'", missing), collapse = ", ")
    ),
    call. = FALSE
  )
}

load_list_by_wave <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Missing list_by_wave.RData at %s. Stage real data into data/raw/ or proxy data into data/proxy/ before Chapter 7.", path), call. = FALSE)
  }

  env <- new.env(parent = emptyenv())
  loaded <- load(path, envir = env)
  if (!"list_by_wave" %in% loaded) {
    stop("list_by_wave object not found in raw data bundle", call. = FALSE)
  }

  get("list_by_wave", envir = env)
}

extract_unique_rows <- function(df, key_col) {
  keep <- !duplicated(df[[key_col]])
  df[keep, , drop = FALSE]
}

build_wave_friends <- function(wave_df, ids, max_friends = 10) {
  friend_cols <- paste0("friendid", seq_len(max_friends))
  mat <- matrix(NA_character_, nrow = length(ids), ncol = max_friends)
  rownames(mat) <- as.character(ids)
  colnames(mat) <- friend_cols

  for (i in seq_len(length(ids))) {
    id <- ids[i]
    rows <- wave_df[wave_df$redcap_survey_identifier == id, , drop = FALSE]
    if (nrow(rows) == 0) {
      next
    }
    for (r in seq_len(nrow(rows))) {
      slot_raw <- rows$which_friendid[r]
      if (is.na(slot_raw)) {
        next
      }
      slot_chr <- as.character(slot_raw)
      if (!nzchar(slot_chr)) {
        next
      }

      slot_idx <- NA_integer_
      if (is.numeric(slot_raw) || is.integer(slot_raw)) {
        slot_idx <- as.integer(slot_raw)
      } else if (slot_chr %in% friend_cols) {
        slot_idx <- suppressWarnings(as.integer(sub("^friendid", "", slot_chr)))
      } else {
        slot_idx <- suppressWarnings(as.integer(gsub("\\D+", "", slot_chr)))
      }

      if (!is.na(slot_idx) && slot_idx >= 1 && slot_idx <= max_friends) {
        mat[i, slot_idx] <- as.character(rows$nomination[r])
      }
    }
  }

  mat
}

compute_wave_observed_senders <- function(wave_df, ids) {
  if (is.null(wave_df) || nrow(wave_df) == 0) {
    return(integer(0))
  }

  observed <- logical(length(ids))
  has_friend_number <- "friend_number" %in% names(wave_df)

  for (i in seq_along(ids)) {
    id <- ids[i]
    rows <- wave_df[wave_df$redcap_survey_identifier == id, , drop = FALSE]
    if (nrow(rows) == 0) {
      next
    }

    nominations <- suppressWarnings(as.integer(rows$nomination))
    has_valid_nomination <- any(!is.na(nominations) & nominations %in% ids)
    has_explicit_zero <- FALSE

    if (has_friend_number) {
      friend_number <- suppressWarnings(as.numeric(rows$friend_number))
      has_explicit_zero <- any(!is.na(friend_number) & friend_number == 0)
    }

    observed[i] <- has_valid_nomination || has_explicit_zero
  }

  ids[observed]
}

make_network_array <- function(ids, friend_matrices, observed_senders = NULL) {
  # Create array with exactly as many waves as friend_matrices
  # (Don't prepend an empty wave 1 - that causes SAOM rate explosion)
  waves <- length(friend_matrices)
  arr <- array(NA_integer_, dim = c(length(ids), length(ids), waves))
  dimnames(arr) <- list(as.character(ids), as.character(ids), paste0("wave", seq_len(waves)))

  for (w in seq_along(friend_matrices)) {
    friends <- friend_matrices[[w]]
    if (is.null(friends)) {
      next
    }

    wave_observed <- ids
    if (!is.null(observed_senders) && length(observed_senders) >= w) {
      candidate <- observed_senders[[w]]
      if (!is.null(candidate) && length(candidate)) {
        wave_observed <- intersect(ids, candidate)
      }
    }
    observed_idx <- match(wave_observed, ids)
    observed_idx <- observed_idx[!is.na(observed_idx)]
    if (length(observed_idx)) {
      # For actors observed at this wave, treat all non-nominated ties as 0.
      arr[observed_idx, , w] <- 0L
    }

    for (i in seq_len(nrow(friends))) {
      nominations <- friends[i, ]
      nominations <- nominations[!is.na(nominations) & nzchar(nominations)]
      if (length(nominations) == 0) {
        next
      }
      sender_idx <- i
      receivers <- suppressWarnings(as.integer(nominations))
      valid <- !is.na(receivers) & receivers %in% ids
      if (!any(valid)) {
        next
      }
      receiver_idx <- match(receivers[valid], ids)
      # Put in column w (not w+1) - no empty wave 1 prepended
      arr[cbind(sender_idx, receiver_idx, rep(w, length(receiver_idx)))] <- 1L
    }

    if (length(observed_idx)) {
      arr[cbind(observed_idx, observed_idx, rep(w, length(observed_idx)))] <- 0L
    }
  }

  arr
}

make_behaviour_matrix <- function(ids, base_wave, wave_data_list) {
  # Create matrix with exactly as many waves as wave_data_list
  # (Matches the network array which also has length(friend_matrices) waves)
  waves <- length(wave_data_list)
  mat <- matrix(NA_real_, nrow = length(ids), ncol = waves)
  dimnames(mat) <- list(as.character(ids), paste0("wave", seq_len(waves)))

  for (w in seq_along(wave_data_list)) {
    wave_df <- extract_unique_rows(wave_data_list[[w]], "redcap_survey_identifier")
    ord <- match(ids, wave_df$redcap_survey_identifier)
    mat[, w] <- wave_df$audit_score[ord]
  }

  mat
}

create_actor_covariates <- function(base_wave, ids) {
  ord <- match(ids, base_wave$redcap_survey_identifier)
  n <- length(ids)
  majority_values <- rep(NA_real_, n)
  if ("majority_status" %in% names(base_wave)) {
    raw <- base_wave$majority_status[ord]
    if (is.numeric(raw) || is.integer(raw)) {
      majority_values <- as.numeric(raw)
    } else {
      raw_chr <- tolower(as.character(raw))
      majority_values <- ifelse(
        is.na(raw_chr),
        NA_real_,
        ifelse(
          raw_chr %in% c("majority", "white", "1", "true", "yes"),
          1,
          ifelse(raw_chr %in% c("minority", "nonwhite", "0", "false", "no"), 0, NA_real_)
        )
      )
    }
  } else {
    if_white <- rep(NA_real_, n)
    if ("if_white" %in% names(base_wave)) {
      raw <- suppressWarnings(as.numeric(base_wave$if_white[ord]))
      if_white <- ifelse(is.na(raw), NA_real_, as.numeric(raw == 1))
    } else if ("ethnicity" %in% names(base_wave)) {
      raw <- suppressWarnings(as.numeric(base_wave$ethnicity[ord]))
      if_white <- ifelse(is.na(raw), NA_real_, as.numeric(raw == 4))
    }

    restrictive_religion_codes <- c(3, 4, 6, 7) # Buddhism, Hinduism, Islam, Sikhism (thesis Chapter 7)
    religion_ok <- rep(NA_real_, n)
    if ("religion" %in% names(base_wave)) {
      raw <- suppressWarnings(as.numeric(base_wave$religion[ord]))
      religion_ok <- ifelse(is.na(raw), NA_real_, as.numeric(!(raw %in% restrictive_religion_codes)))
    }

    british_resident <- rep(NA_real_, n)
    if ("nationality_brit" %in% names(base_wave)) {
      raw <- suppressWarnings(as.numeric(base_wave$nationality_brit[ord]))
      british_resident <- ifelse(is.na(raw), NA_real_, as.numeric(raw == 1))
    }

    age_ok <- rep(NA_real_, n)
    if ("age" %in% names(base_wave)) {
      raw <- suppressWarnings(as.numeric(base_wave$age[ord]))
      age_ok <- ifelse(is.na(raw), NA_real_, as.numeric(raw <= 18))
    }

    all_defined <- !is.na(if_white) & !is.na(religion_ok) & !is.na(british_resident) & !is.na(age_ok)
    majority_values <- ifelse(
      all_defined,
      as.numeric(if_white == 1 & religion_ok == 1 & british_resident == 1 & age_ok == 1),
      NA_real_
    )
  }

  sex_values <- rep(NA_real_, n)
  if ("sex" %in% names(base_wave)) {
    raw <- base_wave$sex[ord]
    if (is.numeric(raw) || is.integer(raw)) {
      sex_values <- as.numeric(raw)
    } else {
      raw_chr <- tolower(as.character(raw))
      sex_values <- ifelse(
        is.na(raw_chr),
        NA_real_,
        ifelse(
          raw_chr %in% c("male", "m", "1", "true", "yes"),
          1,
          ifelse(raw_chr %in% c("female", "f", "0", "false", "no"), 0, NA_real_)
        )
      )
    }
  }
  data.frame(
    redcap_survey_identifier = ids,
    majority_status = majority_values,
    sex = sex_values,
    stringsAsFactors = FALSE
  )
}

create_dyadic_covariates <- function(base_wave, ids) {
  n <- length(ids)
  ord <- match(ids, base_wave$redcap_survey_identifier)
  if ("number_block" %in% names(base_wave)) {
    blocks <- suppressWarnings(as.character(base_wave$number_block[ord]))
  } else {
    blocks <- rep(NA_character_, n)
  }
  if ("number_flat" %in% names(base_wave)) {
    flats <- suppressWarnings(as.character(base_wave$number_flat[ord]))
  } else {
    flats <- rep(NA_character_, n)
  }

  has_block <- !is.na(blocks) & nzchar(blocks)
  has_flat <- !is.na(flats) & nzchar(flats)
  block_key <- ifelse(has_block, blocks, NA_character_)
  flat_key <- ifelse(has_block & has_flat, paste(blocks, flats, sep = "::"), NA_character_)

  flatmates <- outer(flat_key, flat_key, function(x, y) as.integer(!is.na(x) & !is.na(y) & nzchar(x) & nzchar(y) & x == y))
  blockmates <- outer(block_key, block_key, function(x, y) as.integer(!is.na(x) & !is.na(y) & nzchar(x) & nzchar(y) & x == y))

  diag(flatmates) <- 0L
  diag(blockmates) <- 0L
  rownames(flatmates) <- colnames(flatmates) <- as.character(ids)
  rownames(blockmates) <- colnames(blockmates) <- as.character(ids)

  list(flatmates = flatmates, blockmates = blockmates)
}

main <- function() {
  repo_root <- get_repo_root()
  ensure_runtime_packages(c("yaml", "jsonlite"))

  source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
  source(file.path(repo_root, "R", "common.R"))

  cfg <- load_configuration(file.path(repo_root, "config", "thesis.yml"))
  output_paths <- resolve_chapter_output_paths(cfg, "chapter4_data_collection")
  network_path <- file.path(output_paths$data, "network_arrays.rds")

  raw_data_dir <- resolve_data_dir(cfg)
  list_by_wave_path <- file.path(raw_data_dir, "list_by_wave.RData")
  list_by_wave <- load_list_by_wave(list_by_wave_path)
  synthetic_source <- attr(list_by_wave, "synthetic_source", exact = TRUE)
  placeholder_inputs <- identical(synthetic_source, "chapter7_placeholder")

  base_wave <- list_by_wave[[1]]
  # Thesis-aligned waves: baseline + waves 2, 4, 5, 6 (network waves = 2,4,5,6)
  # This matches the thesis time points (Oct, Dec, Mar, next Oct) and yields 3 periods.
  wave_data_list <- list(list_by_wave[[2]], list_by_wave[[4]], list_by_wave[[5]], list_by_wave[[6]])
  observed_wave_ids <- unique(unlist(lapply(wave_data_list, function(df) df$redcap_survey_identifier)))
  observed_wave_ids <- observed_wave_ids[!is.na(observed_wave_ids)]
  baseline_ids <- base_wave$redcap_survey_identifier
  baseline_ids <- baseline_ids[!is.na(baseline_ids)]
  full_participant_ids <- baseline_ids
  included_partial_observed_count <- 0L
  if ("alcohol_use_complete" %in% names(base_wave)) {
    alcohol_complete <- suppressWarnings(as.numeric(base_wave$alcohol_use_complete))
    eligible <- !is.na(alcohol_complete) & alcohol_complete == 2
    eligible_ids <- base_wave$redcap_survey_identifier[eligible]
    eligible_ids <- eligible_ids[!is.na(eligible_ids)]
    if (length(eligible_ids) > 0) {
      observed_partial_ids <- setdiff(intersect(observed_wave_ids, baseline_ids), eligible_ids)
      full_participant_ids <- c(eligible_ids, observed_partial_ids)
      included_partial_observed_count <- length(unique(observed_partial_ids))
    } else {
      warning(
        "No IDs matched `alcohol_use_complete == 2`; falling back to all baseline IDs.",
        call. = FALSE
      )
    }
  }
  # ── Actor ordering ──────────────────────────────────────────────────

  # Legacy behaviour: actors appear in wave-1 natural (unsorted) order.
  # The reproduced pipeline preserves this to keep network matrices and
  # dyadic covariates aligned with the legacy SAOM environment.
  ids <- unique(full_participant_ids)
  excluded_partial_count <- length(unique(baseline_ids)) - length(ids)

  friend_matrices <- lapply(wave_data_list, build_wave_friends, ids = ids, max_friends = 10)
  observed_senders <- lapply(wave_data_list, compute_wave_observed_senders, ids = ids)
  network_array <- make_network_array(ids, friend_matrices, observed_senders = observed_senders)

  behaviour_matrix <- make_behaviour_matrix(ids, base_wave, wave_data_list)

  # ── Actor covariates (legacy-aligned) ─────────────────────────────
  # KNOWN LEGACY BUG: the original analysis built actor covariates
  # (majority_status, sex) in SAOM_outcome_first row order, which
  # differs from the network actor order (wave-1 natural order).
  # RSiena received the covariates positionally, so each actor's
  # network row was paired with a *different* actor's covariate values.
  # The thesis results were estimated under this scrambled mapping.
  #
  # To reproduce the published coefficients we load the legacy

  # covariate vectors (extracted from 10_05_final/) and apply them
  # in the same scrambled positional order.  When the legacy file is
  # absent (e.g. proxy-data runs) we fall back to correctly-aligned
  # covariates computed from the raw fields.
  legacy_cov_path <- file.path(raw_data_dir, "legacy_saom_covariates.rds")
  if (file.exists(legacy_cov_path)) {
    legacy_cov <- readRDS(legacy_cov_path)
    message("[chapter7] Using legacy covariate vectors (scrambled ordering) for thesis reproduction")
    # The legacy vectors are length-248 in SAOM_outcome_first order.
    # We place them positionally against the network actor order (ids)
    # to replicate the original misalignment.
    n <- length(ids)
    actor_covariates <- data.frame(
      redcap_survey_identifier = ids,
      majority_status = legacy_cov$majority_status[seq_len(n)],
      sex             = legacy_cov$sex[seq_len(n)],
      stringsAsFactors = FALSE
    )
  } else {
    message("[chapter7] Legacy covariate file not found; computing covariates from raw fields (correctly aligned)")
    actor_covariates <- create_actor_covariates(base_wave, ids)
  }
  dyadic_covariates <- create_dyadic_covariates(base_wave, ids)

  legacy_covariates_used <- file.exists(legacy_cov_path)
  network_tie_total <- sum(network_array, na.rm = TRUE)
  metadata <- list(
    list_by_wave_path = normalizePath(list_by_wave_path, winslash = "/", mustWork = FALSE),
    synthetic_source = synthetic_source,
    placeholder = placeholder_inputs,
    legacy_covariates = legacy_covariates_used,
    actor_ordering = "wave1_natural",
    statistics = list(
      students = length(ids),
      waves = ncol(behaviour_matrix),
      network_ties = if (is.na(network_tie_total)) NULL else network_tie_total,
      dyadic_nonzero_counts = vapply(
        dyadic_covariates,
        function(value) sum(value != 0, na.rm = TRUE),
        integer(1)
      ),
      observed_senders = vapply(observed_senders, length, integer(1)),
      excluded_partial_participants = excluded_partial_count,
      included_partial_observed_participants = included_partial_observed_count
    )
  )

  payload <- list(
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    placeholder = placeholder_inputs,
    metadata = metadata,
    ids = ids,
    waves = paste0("wave", seq_len(ncol(behaviour_matrix))),
    network_array = network_array,
    behaviour_array = behaviour_matrix,
    actor_covariates = actor_covariates,
    dyadic_covariates = dyadic_covariates
  )

  saveRDS(payload, network_path)
  message("Network arrays regenerated at ", network_path)
}

if (identical(environment(), globalenv())) {
  tryCatch(main(), error = function(err) {
    message("Error rebuilding network arrays: ", err$message)
    quit(status = 1)
  })
}
