#!/usr/bin/env Rscript
# ==============================================================================
# REALISTIC PROXY DATA GENERATOR FOR SAND THESIS REPRODUCTION
# ==============================================================================
#
# This script generates synthetic longitudinal network data that matches the
# structure of the SAND thesis study while having sufficient density and
# realistic properties for RSiena SAOM models to converge.
#
# Key improvements over create_sample_chapter4_raw_data.R:
# - 250 participants (vs 12) to match real study scale
# - 6 waves with realistic attrition (~15% dropout per wave)
# - Homophily-driven friendship formation (residence_cluster, audit_score)
# - Reciprocity structure (~30% reciprocal ties)
# - Temporal autocorrelation in behavior (audit_score stability ~0.6)
# - Network transitivity (friends of friends become friends)
# - Realistic AUDIT-C score distributions from literature
#
# Usage:
#   Rscript create_realistic_proxy_data.R [output_dir]
#
# ==============================================================================

# Configuration constants - derived from thesis.yml and real study expectations
CONFIG <- list(
  # Sample size and structure
  n_participants = 255,           # Match real study (~255, config says min 240)
  n_waves = 6,                    # Real study had 6 waves
  n_residence_clusters = 12,      # Synthetic blocks used for residence homophily
  n_flats_per_cluster = 6,        # Balanced synthetic flats within each block
  max_friend_nominations = 10,    # Max friends nominated per wave
  
  # Attrition parameters (real study: 87% overall retention, ~2-3% per wave)
  base_attrition_rate = 0.03,     # ~3% dropout per wave (was 12%)
  attrition_jitter = 0.01,        # ±1% variance
  
  # Network parameters (calibrated to match real study: Table 7.1)
  target_nominations_mean = 1.8,  # Lower initial; reciprocity enforcement roughly doubles effective degree
  target_nominations_sd = 1.2,    # Tighter SD
  reciprocity_rate = 0.55,        # Match real 0.47-0.58 range
  transitivity_rate = 0.50,       # Match real 0.48-0.53 range
  homophily_residence = 0.40,     # 40% boost for same residence cluster
  homophily_audit_decay = 5,      # AUDIT score difference decay constant
  
  # Tie persistence settings (critical for SAOM convergence)
  tie_persistence_rate = 0.95,    # Target Jaccard 0.66-0.81 (very high for network stability)
  new_tie_rate = 0.25,            # Fraction of potential new ties to consider each wave
  
  # Post-hoc reciprocity/transitivity enforcement
  # These add ties after initial generation to match observed network statistics
  reciprocity_enforcement = 0.50, # Probability of reciprocating an incoming tie
  transitivity_enforcement = 0.05, # Probability of closing a transitive triad
  
  # Behavioral parameters (AUDIT-C, calibrated to match real study)
  audit_mean_initial = 4.8,       # Match baseline AUDIT-C (real study: 4.8)
  audit_sd_initial = 2.8,         # Match baseline SD (real study: 2.8)
  audit_min = 0,
  audit_max = 12,                 # AUDIT-C range (three items scored 0-4)
  audit_autocorr = 0.85,          # Higher temporal stability (was 0.80)
  peer_influence_strength = 0.10, # Slight peer influence (was 0.15)
  
  # Demographics
  sex_female_prop = 0.55,         # 55% female (typical university sample)
  age_mean = 18.8,
  age_sd = 1.2,
  ethnicity_probs = c(0.70, 0.12, 0.10, 0.08),  # Majority, minority groups
  majority_prop = 0.72,           # 72% vs 28% minority status
  
  # Injunctive norms (inno1-3: approval of abstaining, binge, passing out)
  inno_means = c(2.5, 1.8, 1.2),  # Decreasing approval for riskier behavior
  inno_sd = 1.0,
  
  # Descriptive norms (deno1, deno3, deno4)
  deno_means = c(3.0, 2.5, 2.0),
  deno_sd = 1.2,
  
  # Seeds for reproducibility
  master_seed = 20250921          # Match thesis project seed
)

# ==============================================================================
# Helper Functions
# ==============================================================================

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
  stop("Unable to determine script path for realistic proxy data generator.")
}

resolve_output_dir <- function(arg_path, repo_root) {
  if (is.null(arg_path) || !nzchar(arg_path)) {
    return(file.path(repo_root, "data", "raw"))
  }
  if (grepl("^/", arg_path) || grepl("^[A-Za-z]:\\\\", arg_path)) {
    return(normalizePath(arg_path, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(repo_root, arg_path), winslash = "/", mustWork = FALSE)
}

clamp <- function(x, min_val, max_val) {
  pmax(min_val, pmin(max_val, x))
}

generate_residence_assignments <- function(n) {
  blocks <- sample(rep(seq_len(CONFIG$n_residence_clusters), length.out = n))
  flats <- integer(n)
  for (block in unique(blocks)) {
    members <- which(blocks == block)
    flats[members] <- sample(
      rep(seq_len(CONFIG$n_flats_per_cluster), length.out = length(members))
    )
  }

  data.frame(
    residence_cluster = blocks,
    number_block = blocks,
    number_flat = flats,
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Demographics Generator
# ==============================================================================

generate_demographics <- function(n, seed_offset = 0) {
  set.seed(CONFIG$master_seed + seed_offset)

  residence <- generate_residence_assignments(n)
  demographics <- data.frame(
    redcap_survey_identifier = 1001:(1000 + n),
    age = round(clamp(rnorm(n, CONFIG$age_mean, CONFIG$age_sd), 17, 25)),
    sex = sample(0:1, n, replace = TRUE, prob = c(CONFIG$sex_female_prop, 1 - CONFIG$sex_female_prop)),
    ethnicity = sample(1:4, n, replace = TRUE, prob = CONFIG$ethnicity_probs),
    majority_status = sample(0:1, n, replace = TRUE, prob = c(1 - CONFIG$majority_prop, CONFIG$majority_prop)),
    stringsAsFactors = FALSE
  )
  cbind(demographics, residence)
}

# ==============================================================================
# Behavioral Variables Generator (AUDIT-C, BYAACQ)
# ==============================================================================

generate_initial_behavior <- function(demographics, seed_offset = 0) {
  set.seed(CONFIG$master_seed + seed_offset + 100)
  n <- nrow(demographics)
  
  # AUDIT-C components (q1: frequency, q2: quantity, q3: binge frequency)
  # Literature: q1,q2,q3 each 0-4 for AUDIT-C
  q1 <- round(clamp(rnorm(n, 2.0, 1.2), 0, 4))  # Drinking frequency
  q2 <- round(clamp(rnorm(n, 1.5, 1.3), 0, 4))  # Typical quantity
  q3 <- round(clamp(rnorm(n, 1.2, 1.4), 0, 4))  # Binge frequency
  
  audit_score <- q1 + q2 + q3  # 0-12 for AUDIT-C
  
  # BYAACQ (Brief Young Adult Alcohol Consequences Questionnaire)
  # Short form is 0-6 for byaacq_6
  byaacq_6 <- round(clamp(rnorm(n, 2.0, 1.8), 0, 6))
  
  # Self-reported injunctive norms (approval ratings 0-4)
  inno1_self <- round(clamp(rnorm(n, CONFIG$inno_means[1], CONFIG$inno_sd), 0, 4))
  inno2_self <- round(clamp(rnorm(n, CONFIG$inno_means[2], CONFIG$inno_sd), 0, 4))
  inno3_self <- round(clamp(rnorm(n, CONFIG$inno_means[3], CONFIG$inno_sd), 0, 4))
  
  data.frame(
    redcap_survey_identifier = demographics$redcap_survey_identifier,
    q1 = q1,
    q2 = q2,
    q3 = q3,
    audit_score = audit_score,
    byaacq_6 = byaacq_6,
    inno1_self = inno1_self,
    inno2_self = inno2_self,
    inno3_self = inno3_self,
    stringsAsFactors = FALSE
  )
}

evolve_behavior <- function(prev_behavior, network_ties, all_behavior, wave = NULL, seed_offset = 0) {
  set.seed(CONFIG$master_seed + seed_offset + 200)
  n <- nrow(prev_behavior)
  
  # Calculate peer average AUDIT scores for influence
  peer_avg <- sapply(prev_behavior$redcap_survey_identifier, function(id) {
    friends <- network_ties$nomination[network_ties$redcap_survey_identifier == id]
    friends <- friends[!is.na(friends)]
    if (length(friends) == 0) return(mean(all_behavior$audit_score, na.rm = TRUE))
    friend_scores <- all_behavior$audit_score[all_behavior$redcap_survey_identifier %in% friends]
    if (length(friend_scores) == 0) return(mean(all_behavior$audit_score, na.rm = TRUE))
    mean(friend_scores, na.rm = TRUE)
  })
  
  # Evolve AUDIT score with "Freshers' spike" for wave 2
  # Real study: baseline 4.8 -> Time 1 (wave 2) 7.4 -> stabilizes 6.4-6.7
  if (!is.null(wave) && wave == 2) {
    # Freshers' spike: increase by ~2.6 points (4.8 -> 7.4)
    new_audit <- prev_behavior$audit_score + 2.6 + rnorm(n, 0, 1.0)
  } else {
    # Normal evolution: autocorrelation + peer influence + noise
    new_audit <- CONFIG$audit_autocorr * prev_behavior$audit_score +
                 CONFIG$peer_influence_strength * peer_avg +
                 rnorm(n, 0, 1.5)
  }
  new_audit <- round(clamp(new_audit, 0, 12))
  
  # Derive q1,q2,q3 from total (approximately)
  q1 <- round(clamp(new_audit * 0.35 + rnorm(n, 0, 0.5), 0, 4))
  q2 <- round(clamp(new_audit * 0.30 + rnorm(n, 0, 0.5), 0, 4))
  q3 <- new_audit - q1 - q2
  q3 <- round(clamp(q3, 0, 4))
  
  # Recalculate audit_score to be consistent
  audit_score <- q1 + q2 + q3
  
  # BYAACQ evolves with some correlation to audit
  byaacq_6 <- round(clamp(0.5 * prev_behavior$byaacq_6 + 0.3 * (audit_score / 12) * 6 + rnorm(n, 0, 1), 0, 6))
  
  # Injunctive norms evolve slowly
  inno1_self <- round(clamp(0.8 * prev_behavior$inno1_self + 0.2 * CONFIG$inno_means[1] + rnorm(n, 0, 0.3), 0, 4))
  inno2_self <- round(clamp(0.8 * prev_behavior$inno2_self + 0.2 * CONFIG$inno_means[2] + rnorm(n, 0, 0.3), 0, 4))
  inno3_self <- round(clamp(0.8 * prev_behavior$inno3_self + 0.2 * CONFIG$inno_means[3] + rnorm(n, 0, 0.3), 0, 4))
  
  data.frame(
    redcap_survey_identifier = prev_behavior$redcap_survey_identifier,
    q1 = q1,
    q2 = q2,
    q3 = q3,
    audit_score = audit_score,
    byaacq_6 = byaacq_6,
    inno1_self = inno1_self,
    inno2_self = inno2_self,
    inno3_self = inno3_self,
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Network Generator with Homophily, Transitivity, and Tie Persistence
# ==============================================================================

generate_network_ties <- function(active_ids, demographics, behavior, prev_network = NULL, wave = 1, seed_offset = 0) {
  set.seed(CONFIG$master_seed + seed_offset + 300)
  
  n <- length(active_ids)
  all_ties <- vector("list", n)
  
  # Apply wave-based decay to nominations (real study: 4.0 -> 2.3 over waves)
  # 10% reduction per wave after wave 2
  wave_decay <- if (wave > 2) 1 - (wave - 2) * 0.10 else 1.0
  wave_decay <- max(wave_decay, 0.5)  # Floor at 50% of original
  
  for (i in seq_len(n)) {
    ego_id <- active_ids[i]
    ego_row <- demographics[demographics$redcap_survey_identifier == ego_id, ]
    ego_behavior <- behavior[behavior$redcap_survey_identifier == ego_id, ]
    
    # Determine target number of nominations with wave decay
    base_target <- rnorm(1, CONFIG$target_nominations_mean, CONFIG$target_nominations_sd)
    n_friends_target <- round(clamp(base_target * wave_decay, 0, CONFIG$max_friend_nominations))
    
    if (n_friends_target == 0) {
      all_ties[[i]] <- data.frame(
        redcap_survey_identifier = ego_id,
        which_friendid = NA_integer_,
        nomination = NA_integer_,
        stringsAsFactors = FALSE
      )
      next
    }
    
    # ============================================
    # TIE PERSISTENCE: Keep existing ties with high probability
    # ============================================
    kept_friends <- integer(0)
    
    if (!is.null(prev_network)) {
      # Get existing friends from previous wave
      existing_friends <- prev_network$nomination[prev_network$redcap_survey_identifier == ego_id]
      existing_friends <- existing_friends[!is.na(existing_friends)]
      # Only keep friends who are still active
      existing_friends <- existing_friends[existing_friends %in% active_ids]
      
      if (length(existing_friends) > 0) {
        # Per-tie persistence: each existing tie survives with probability = tie_persistence_rate
        # This gives expected Jaccard much closer to the persistence rate
        keep_mask <- runif(length(existing_friends)) < CONFIG$tie_persistence_rate
        kept_friends <- existing_friends[keep_mask]
      }
    }
    
    # Calculate how many NEW friends to add (target minus kept)
    n_new_needed <- max(0, n_friends_target - length(kept_friends))
    
    # If we kept more than target, that's fine - prioritize stability over exact count
    # But cap at max_friend_nominations
    if (length(kept_friends) > CONFIG$max_friend_nominations) {
      kept_friends <- sample(kept_friends, CONFIG$max_friend_nominations)
      n_new_needed <- 0
    }
    
    # If no new friends needed, we're done
    if (n_new_needed <= 0) {
      if (length(kept_friends) > 0) {
        all_ties[[i]] <- data.frame(
          redcap_survey_identifier = rep(ego_id, length(kept_friends)),
          which_friendid = seq_along(kept_friends),
          nomination = kept_friends,
          stringsAsFactors = FALSE
        )
      } else {
        all_ties[[i]] <- data.frame(
          redcap_survey_identifier = ego_id,
          which_friendid = NA_integer_,
          nomination = NA_integer_,
          stringsAsFactors = FALSE
        )
      }
      next
    }
    
    # ============================================
    # NEW TIE FORMATION: Sample new friends
    # ============================================
    potential_alters <- active_ids[active_ids != ego_id]
    # Exclude friends we're already keeping
    potential_alters <- potential_alters[!potential_alters %in% kept_friends]
    n_alters <- length(potential_alters)
    
    if (n_alters == 0) {
      # No potential new friends, just use kept friends
      if (length(kept_friends) > 0) {
        all_ties[[i]] <- data.frame(
          redcap_survey_identifier = rep(ego_id, length(kept_friends)),
          which_friendid = seq_along(kept_friends),
          nomination = kept_friends,
          stringsAsFactors = FALSE
        )
      } else {
        all_ties[[i]] <- data.frame(
          redcap_survey_identifier = ego_id,
          which_friendid = NA_integer_,
          nomination = NA_integer_,
          stringsAsFactors = FALSE
        )
      }
      next
    }
    
    tie_probs <- rep(1, n_alters)
    
    for (j in seq_len(n_alters)) {
      alter_id <- potential_alters[j]
      alter_row <- demographics[demographics$redcap_survey_identifier == alter_id, ]
      alter_behavior <- behavior[behavior$redcap_survey_identifier == alter_id, ]
      
      # Homophily on residence cluster
      if (nrow(ego_row) > 0 && nrow(alter_row) > 0) {
        if (ego_row$residence_cluster == alter_row$residence_cluster) {
          tie_probs[j] <- tie_probs[j] * (1 + CONFIG$homophily_residence)
        }
      }
      
      # Homophily on AUDIT score (similar drinkers attract)
      if (nrow(ego_behavior) > 0 && nrow(alter_behavior) > 0) {
        audit_diff <- abs(ego_behavior$audit_score - alter_behavior$audit_score)
        tie_probs[j] <- tie_probs[j] * exp(-audit_diff / CONFIG$homophily_audit_decay)
      }
      
      # Reciprocity boost: if alter nominated ego in previous wave
      if (!is.null(prev_network)) {
        alter_nominated_ego <- any(
          prev_network$redcap_survey_identifier == alter_id &
          prev_network$nomination == ego_id,
          na.rm = TRUE
        )
        if (alter_nominated_ego) {
          tie_probs[j] <- tie_probs[j] * (1 + 2 * CONFIG$reciprocity_rate)
        }
      }
      
      # Transitivity: friends of friends
      if (!is.null(prev_network)) {
        ego_friends <- prev_network$nomination[prev_network$redcap_survey_identifier == ego_id]
        ego_friends <- ego_friends[!is.na(ego_friends)]
        
        is_friend_of_friend <- any(
          prev_network$redcap_survey_identifier %in% ego_friends &
          prev_network$nomination == alter_id,
          na.rm = TRUE
        )
        if (is_friend_of_friend) {
          tie_probs[j] <- tie_probs[j] * (1 + CONFIG$transitivity_rate)
        }
      }
    }
    
    # Normalize and sample NEW friends
    tie_probs <- tie_probs / sum(tie_probs)
    n_to_sample <- min(n_new_needed, n_alters)
    
    new_friends <- integer(0)
    if (n_to_sample > 0) {
      new_friends <- sample(potential_alters, n_to_sample, replace = FALSE, prob = tie_probs)
    }
    
    # Combine kept and new friends
    all_selected <- c(kept_friends, new_friends)
    
    if (length(all_selected) > 0) {
      all_ties[[i]] <- data.frame(
        redcap_survey_identifier = rep(ego_id, length(all_selected)),
        which_friendid = seq_along(all_selected),
        nomination = all_selected,
        stringsAsFactors = FALSE
      )
    } else {
      all_ties[[i]] <- data.frame(
        redcap_survey_identifier = ego_id,
        which_friendid = NA_integer_,
        nomination = NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  }
  
  do.call(rbind, all_ties)
}

# ==============================================================================
# Post-Hoc Reciprocity and Transitivity Enforcement
# ==============================================================================
#
# The initial network generation produces ties via probabilistic sampling, but
# the resulting reciprocity and transitivity are typically much lower than
# observed in real social networks. This function adds ties to bring these
# statistics closer to the thesis-reported values (reciprocity ~0.47-0.58,
# transitivity ~0.28-0.53).
#
# Strategy:
#   1. For each non-reciprocated tie (i->j but not j->i), add j->i with
#      probability = reciprocity_enforcement, subject to max_friend_nominations.
#   2. For each open triad (i->j, j->k, but not i->k), close it with
#      probability = transitivity_enforcement, subject to max_friend_nominations.

enforce_reciprocity_transitivity <- function(network_df, active_ids, seed_offset = 0) {
  set.seed(CONFIG$master_seed + seed_offset + 700)

  # Build adjacency list for fast lookup
  edges <- network_df[!is.na(network_df$nomination), c("redcap_survey_identifier", "nomination")]
  adj <- split(edges$nomination, edges$redcap_survey_identifier)

  # Count current nominations per ego
  nom_count <- table(edges$redcap_survey_identifier)

  new_ties <- list()
  tie_idx <- 0L

  # --- Reciprocity enforcement ---
  for (i in seq_len(nrow(edges))) {
    sender <- edges$redcap_survey_identifier[i]
    receiver <- edges$nomination[i]

    # Check if reverse tie exists
    receiver_friends <- adj[[as.character(receiver)]]
    if (!is.null(receiver_friends) && sender %in% receiver_friends) next

    # Check if receiver has room for more nominations
    receiver_count <- as.integer(nom_count[as.character(receiver)])
    if (is.na(receiver_count)) receiver_count <- 0L
    if (receiver_count >= CONFIG$max_friend_nominations) next

    # Add reciprocal tie with probability
    if (runif(1) < CONFIG$reciprocity_enforcement) {
      tie_idx <- tie_idx + 1L
      new_ties[[tie_idx]] <- data.frame(
        redcap_survey_identifier = receiver,
        which_friendid = receiver_count + 1L,
        nomination = sender,
        stringsAsFactors = FALSE
      )
      # Update tracking
      nom_count[as.character(receiver)] <- receiver_count + 1L
      adj[[as.character(receiver)]] <- c(receiver_friends, sender)
    }
  }

  # --- Transitivity enforcement ---
  # For each ego, look at friends-of-friends and close triads
  for (ego_chr in names(adj)) {
    ego <- as.integer(ego_chr)
    ego_friends <- adj[[ego_chr]]
    if (is.null(ego_friends) || length(ego_friends) == 0) next

    ego_count <- as.integer(nom_count[ego_chr])
    if (is.na(ego_count)) ego_count <- 0L
    if (ego_count >= CONFIG$max_friend_nominations) next

    # Collect friends-of-friends (potential transitive ties)
    fof <- unique(unlist(lapply(as.character(ego_friends), function(f) adj[[f]])))
    fof <- fof[!is.na(fof)]
    # Remove ego and existing friends
    fof <- setdiff(fof, c(ego, ego_friends))
    # Only consider active participants
    fof <- fof[fof %in% active_ids]

    if (length(fof) == 0) next

    for (candidate in fof) {
      if (ego_count >= CONFIG$max_friend_nominations) break
      if (runif(1) < CONFIG$transitivity_enforcement) {
        tie_idx <- tie_idx + 1L
        ego_count <- ego_count + 1L
        new_ties[[tie_idx]] <- data.frame(
          redcap_survey_identifier = ego,
          which_friendid = ego_count,
          nomination = candidate,
          stringsAsFactors = FALSE
        )
        nom_count[ego_chr] <- ego_count
        adj[[ego_chr]] <- c(adj[[ego_chr]], candidate)
      }
    }
  }

  if (tie_idx > 0) {
    added <- do.call(rbind, new_ties)
    network_df <- rbind(network_df, added)
    message(sprintf("    Enforcement: +%d reciprocal/transitive ties added", tie_idx))
  }

  # Re-number which_friendid per ego
  network_df <- do.call(rbind, lapply(split(network_df, network_df$redcap_survey_identifier), function(df) {
    df <- df[!is.na(df$nomination), ]
    if (nrow(df) == 0) {
      return(data.frame(
        redcap_survey_identifier = df$redcap_survey_identifier[1],
        which_friendid = NA_integer_,
        nomination = NA_integer_,
        stringsAsFactors = FALSE
      ))
    }
    # Deduplicate ties
    df <- df[!duplicated(paste(df$redcap_survey_identifier, df$nomination)), ]
    # Cap at max nominations
    if (nrow(df) > CONFIG$max_friend_nominations) {
      df <- df[seq_len(CONFIG$max_friend_nominations), ]
    }
    df$which_friendid <- seq_len(nrow(df))
    df
  }))
  rownames(network_df) <- NULL
  network_df
}

# ==============================================================================
# Friend Perception Columns Generator
# ==============================================================================

FRIEND_INDICES <- 0:10
FRIEND_PREFIXES <- c("deno1_friend_", "deno3_friend_", "deno4_friend_",
                     "inno1_friend_", "inno2_friend_", "inno3_friend_")
FRIEND_COLUMNS <- as.vector(outer(FRIEND_PREFIXES, FRIEND_INDICES, paste0))

generate_friend_perceptions <- function(df, seed_offset = 0) {
  set.seed(CONFIG$master_seed + seed_offset + 400)
  n <- nrow(df)
  
  # Generate friend perception columns
  for (col in FRIEND_COLUMNS) {
    if (grepl("^deno", col)) {
      df[[col]] <- sample(0:10, n, replace = TRUE)
    } else {
      df[[col]] <- sample(0:6, n, replace = TRUE)
    }
  }
  
  df
}

# ==============================================================================
# Peer Reference Scores Generator
# ==============================================================================

generate_peer_scores <- function(df, network_df, all_behavior, seed_offset = 0) {
  set.seed(CONFIG$master_seed + seed_offset + 500)
  n <- nrow(df)
  
  # Actual peer scores (based on network)
  actual_audit <- sapply(df$redcap_survey_identifier, function(id) {
    friends <- network_df$nomination[network_df$redcap_survey_identifier == id]
    friends <- friends[!is.na(friends)]
    if (length(friends) == 0) return(round(mean(all_behavior$audit_score, na.rm = TRUE)))
    friend_scores <- all_behavior$audit_score[all_behavior$redcap_survey_identifier %in% friends]
    if (length(friend_scores) == 0) return(round(mean(all_behavior$audit_score, na.rm = TRUE)))
    round(mean(friend_scores, na.rm = TRUE))
  })
  
  df$actual_audit_score_peer <- actual_audit
  df$actual_deno1_peer <- round(clamp(actual_audit * 0.8 + rnorm(n, 0, 1), 0, 12))
  df$actual_deno3_peer <- round(clamp(actual_audit * 0.7 + rnorm(n, 0, 1), 0, 12))
  df$actual_deno4_peer <- round(clamp(actual_audit * 0.6 + rnorm(n, 0, 1), 0, 12))
  df$actual_inno1_peer <- sample(0:6, n, replace = TRUE)
  df$actual_inno2_peer <- sample(0:6, n, replace = TRUE)
  df$actual_inno3_peer <- sample(0:6, n, replace = TRUE)
  
  # Misperception scores (typically positive - overestimate peer drinking)
  df$misperception_audit_score_peer <- sample(-2:3, n, replace = TRUE, prob = c(0.1, 0.15, 0.25, 0.25, 0.15, 0.10))
  df$misperception_q1_peer <- sample(-2:2, n, replace = TRUE)
  df$misperception_q2_peer <- sample(-2:2, n, replace = TRUE)
  df$misperception_q3_peer <- sample(-2:2, n, replace = TRUE)
  df$misperception_inno1_peer <- sample(-2:2, n, replace = TRUE)
  df$misperception_inno2_peer <- sample(-2:2, n, replace = TRUE)
  df$misperception_inno3_peer <- sample(-2:2, n, replace = TRUE)
  
  # Derived peer metrics
  df$deno_audit_peer <- df$actual_audit_score_peer + df$misperception_audit_score_peer
  df$deno1_peer <- df$actual_deno1_peer + df$misperception_q1_peer
  df$deno3_peer <- df$actual_deno3_peer + df$misperception_q2_peer
  df$deno4_peer <- df$actual_deno4_peer + df$misperception_q3_peer
  df$inno1_peer <- df$actual_inno1_peer + df$misperception_inno1_peer
  df$inno2_peer <- df$actual_inno2_peer + df$misperception_inno2_peer
  df$inno3_peer <- df$actual_inno3_peer + df$misperception_inno3_peer
  
  df
}

# ==============================================================================
# Wave Data Assembly
# ==============================================================================

BASE_COLUMNS <- c(
  "redcap_survey_identifier", "redcap_event_name", "age", "sex",
  "ethnicity", "majority_status", "residence_cluster", "number_block",
  "number_flat", "friend_number",
  "q1", "q2", "q3", "audit_score", "byaacq_6",
  "inno1_self", "inno2_self", "inno3_self",
  FRIEND_COLUMNS,
  "actual_audit_score_peer", "actual_deno1_peer", "actual_deno3_peer",
  "actual_deno4_peer", "actual_inno1_peer", "actual_inno2_peer",
  "actual_inno3_peer",
  "misperception_audit_score_peer", "misperception_q1_peer",
  "misperception_q2_peer", "misperception_q3_peer",
  "misperception_inno1_peer", "misperception_inno2_peer",
  "misperception_inno3_peer",
  "deno_audit_peer", "deno1_peer", "deno3_peer",
  "deno4_peer", "inno1_peer", "inno2_peer", "inno3_peer"
)

NOMINATION_COLUMNS <- c(
  "redcap_survey_identifier", "redcap_event_name",
  "which_friendid", "nomination",
  setdiff(BASE_COLUMNS, c("redcap_survey_identifier", "redcap_event_name"))
)

assemble_wave_1 <- function(demographics, behavior, network, seed_offset = 0) {
  wave_df <- merge(demographics, behavior, by = "redcap_survey_identifier")
  
  # Add friend_number from network
  friend_counts <- aggregate(nomination ~ redcap_survey_identifier, data = network, 
                             FUN = function(x) sum(!is.na(x)))
  names(friend_counts)[2] <- "friend_number"
  wave_df <- merge(wave_df, friend_counts, by = "redcap_survey_identifier", all.x = TRUE)
  wave_df$friend_number[is.na(wave_df$friend_number)] <- 0
  
  wave_df$redcap_event_name <- "wave1_arm_1"
  
  # Add friend perceptions
  wave_df <- generate_friend_perceptions(wave_df, seed_offset)
  
  # Add peer scores
  wave_df <- generate_peer_scores(wave_df, network, behavior, seed_offset)
  
  # Ensure all columns exist and order correctly
  for (col in BASE_COLUMNS) {
    if (!col %in% names(wave_df)) {
      wave_df[[col]] <- NA_integer_
    }
  }
  
  # Wave 1 format: one row per participant (no nomination columns)
  # This is required for Chapter 4 processing compatibility
  # SAOM only reads networks from waves 2+ anyway
  wave_df[, BASE_COLUMNS]
}

assemble_wave_n <- function(wave_num, active_ids, demographics, behavior, network, seed_offset = 0) {
  # Filter to active participants
  demo_active <- demographics[demographics$redcap_survey_identifier %in% active_ids, ]
  behav_active <- behavior[behavior$redcap_survey_identifier %in% active_ids, ]
  
  wave_df <- merge(demo_active, behav_active, by = "redcap_survey_identifier")
  
  # Add friend_number from network
  net_active <- network[network$redcap_survey_identifier %in% active_ids, ]
  friend_counts <- aggregate(nomination ~ redcap_survey_identifier, data = net_active,
                             FUN = function(x) sum(!is.na(x)))
  names(friend_counts)[2] <- "friend_number"
  wave_df <- merge(wave_df, friend_counts, by = "redcap_survey_identifier", all.x = TRUE)
  wave_df$friend_number[is.na(wave_df$friend_number)] <- 0
  
  wave_df$redcap_event_name <- sprintf("wave%d_arm_1", wave_num)
  
  # Add friend perceptions - GENERATE AT PARTICIPANT LEVEL BEFORE MERGE
  wave_df <- generate_friend_perceptions(wave_df, seed_offset + wave_num)
  
  # Add peer scores - GENERATE AT PARTICIPANT LEVEL BEFORE MERGE
  wave_df <- generate_peer_scores(wave_df, net_active, behav_active, seed_offset + wave_num)
  
  # NOW merge with nomination columns (creating multiple rows per participant)
  wave_df <- merge(wave_df, 
                   net_active[, c("redcap_survey_identifier", "which_friendid", "nomination")],
                   by = "redcap_survey_identifier", all = TRUE)
  
  # Ensure all columns exist
  for (col in NOMINATION_COLUMNS) {
    if (!col %in% names(wave_df)) {
      wave_df[[col]] <- NA_integer_
    }
  }
  
  wave_df[, NOMINATION_COLUMNS]
}

# ==============================================================================
# Attrition Simulator
# ==============================================================================

simulate_attrition <- function(active_ids, wave_num, seed_offset = 0) {
  set.seed(CONFIG$master_seed + seed_offset + 600 + wave_num)
  
  # Attrition rate with jitter
  attrition_rate <- CONFIG$base_attrition_rate + runif(1, -CONFIG$attrition_jitter, CONFIG$attrition_jitter)
  n_dropout <- round(length(active_ids) * attrition_rate)
  
  if (n_dropout > 0 && n_dropout < length(active_ids)) {
    dropouts <- sample(active_ids, n_dropout)
    active_ids <- active_ids[!active_ids %in% dropouts]
  }
  
  active_ids
}

# ==============================================================================
# Output Writers
# ==============================================================================

write_participants <- function(base_wave, output_dir) {
  participants <- base_wave[, c(
    "redcap_survey_identifier", "age", "sex", "ethnicity", "majority_status",
    "residence_cluster", "number_block", "number_flat", "friend_number"
  )]
  utils::write.csv(participants, file.path(output_dir, "participants.csv"), row.names = FALSE)
}

write_outcomes <- function(base_wave, output_dir) {
  outcomes <- base_wave[, c("redcap_survey_identifier", "audit_score", "q1", "q2", "q3", "byaacq_6")]
  utils::write.csv(outcomes, file.path(output_dir, "outcomes.csv"), row.names = FALSE)
}

capture_schema <- function(list_by_wave) {
  schema_frames <- lapply(seq_along(list_by_wave), function(idx) {
    wave_df <- list_by_wave[[idx]]
    event_names <- unique(wave_df$redcap_event_name)
    wave_label <- if (length(event_names) == 1) event_names else paste0("wave", idx)
    
    data.frame(
      wave = wave_label,
      column = names(wave_df),
      storage_mode = vapply(wave_df, function(col) typeof(col), character(1L)),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, schema_frames)
}

# ==============================================================================
# Jaccard Index Calculator for Network Stability
# ==============================================================================

calculate_jaccard <- function(net1, net2) {
  # Extract tie pairs from each network
  ties1 <- paste(net1$redcap_survey_identifier, net1$nomination, sep = "->")
  ties2 <- paste(net2$redcap_survey_identifier, net2$nomination, sep = "->")
  # Remove NA ties
  ties1 <- ties1[!grepl("NA", ties1)]
  ties2 <- ties2[!grepl("NA", ties2)]
  
  if (length(ties1) == 0 && length(ties2) == 0) return(1.0)  # Both empty = identical
  if (length(ties1) == 0 || length(ties2) == 0) return(0.0)  # One empty = dissimilar
  
  intersection <- length(intersect(ties1, ties2))
  union_size <- length(union(ties1, ties2))
  
  if (union_size == 0) return(0)
  intersection / union_size
}

# ==============================================================================
# Output Writers
# ==============================================================================

write_generation_report <- function(list_by_wave, output_dir, jaccard_indices = NULL) {
  report <- list(
    generated_at = format(Sys.time(), tz = "UTC"),
    generator = "create_realistic_proxy_data.R",
    purpose = "Realistic proxy data for SAND thesis SAOM convergence testing",
    config = CONFIG,
    wave_summaries = lapply(seq_along(list_by_wave), function(w) {
      wave_df <- list_by_wave[[w]]
      ids <- unique(wave_df$redcap_survey_identifier)
      n_ties <- sum(!is.na(wave_df$nomination)) / length(ids) # avg ties per person
      list(
        wave = w,
        n_participants = length(ids),
        n_rows = nrow(wave_df),
        avg_ties_per_participant = if (w > 1) round(n_ties, 2) else NA,
        mean_audit_score = round(mean(wave_df$audit_score, na.rm = TRUE), 2)
      )
    })
  )
  
  # Add Jaccard indices if provided
  if (!is.null(jaccard_indices)) {
    report$jaccard_indices <- jaccard_indices
  }
  
  jsonlite::write_json(report, file.path(output_dir, "proxy_data_generation_report.json"),
                       pretty = TRUE, auto_unbox = TRUE)
}

# ==============================================================================
# Main Function
# ==============================================================================

main <- function() {
  script_path <- get_script_path()
  script_dir <- dirname(script_path)
  repo_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)
  
  args <- commandArgs(trailingOnly = TRUE)
  output_arg <- if (length(args) >= 1) args[[1]] else NULL
  output_dir <- resolve_output_dir(output_arg, repo_root)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("=== Generating Realistic Proxy Data for SAND Thesis ===")
  message(sprintf("Configuration: %d participants, %d waves", CONFIG$n_participants, CONFIG$n_waves))
  message(sprintf("Output directory: %s", output_dir))
  
  # Initialize all participants
  demographics <- generate_demographics(CONFIG$n_participants, seed_offset = 0)
  behavior <- generate_initial_behavior(demographics, seed_offset = 0)
  active_ids <- demographics$redcap_survey_identifier
  
  # Storage for all waves
  list_by_wave <- vector("list", CONFIG$n_waves)
  prev_network <- NULL
  
  for (wave in 1:CONFIG$n_waves) {
    message(sprintf("Generating wave %d (%d active participants)...", wave, length(active_ids)))
    
    # Generate network ties with wave number for decay
    network <- generate_network_ties(active_ids, demographics, behavior, prev_network, wave = wave, seed_offset = wave * 10)
    
    # Enforce reciprocity and transitivity (waves 2+ only, where nominations exist)
    if (wave > 1) {
      network <- enforce_reciprocity_transitivity(network, active_ids, seed_offset = wave * 10)
    }
    
    if (wave == 1) {
      # Wave 1: base format without nomination columns
      wave_df <- assemble_wave_1(demographics, behavior, network, seed_offset = wave * 10)
    } else {
      # Waves 2-6: include nomination columns
      wave_df <- assemble_wave_n(wave, active_ids, demographics, behavior, network, seed_offset = wave * 10)
    }
    
    list_by_wave[[wave]] <- wave_df
    prev_network <- network
    
    # Simulate attrition for next wave (not after last wave)
    if (wave < CONFIG$n_waves) {
      active_ids <- simulate_attrition(active_ids, wave, seed_offset = wave * 10)
      # Evolve behavior based on peer influence, passing wave+1 for next wave's behavior
      behavior <- evolve_behavior(behavior, network, behavior, wave = wave + 1, seed_offset = wave * 10)
      # Filter behavior to active participants
      behavior <- behavior[behavior$redcap_survey_identifier %in% active_ids, ]
    }
  }
  
  # Calculate network statistics
  total_ties <- sum(sapply(list_by_wave[-1], function(w) sum(!is.na(w$nomination))))
  unique_participants <- length(unique(list_by_wave[[1]]$redcap_survey_identifier))
  
  message(sprintf("Total network ties across waves 2-6: %d", total_ties))
  
  # Calculate Jaccard indices for the SAOM observation waves. Wave 1 has no
  # nomination rows in the public-compatible data structure and must not be
  # compared with Wave 2.
  message("\n=== Network Stability Diagnostics (Jaccard Index) ===")
  message("Target range: 0.30-0.70 | Real study baseline: 0.66-0.81")
  jaccard_indices <- list()
  all_good <- TRUE
  saom_waves <- c(2L, 4L, 5L, 6L)
  saom_pairs <- Map(c, head(saom_waves, -1), tail(saom_waves, -1))
  for (pair in saom_pairs) {
    from_wave <- pair[[1]]
    to_wave <- pair[[2]]
    j <- calculate_jaccard(list_by_wave[[from_wave]], list_by_wave[[to_wave]])
    wave_pair <- sprintf("wave_%d_to_%d", from_wave, to_wave)
    jaccard_indices[[wave_pair]] <- round(j, 3)
    
    if (j >= 0.30 && j <= 0.80) {
      status <- "✓ GOOD"
    } else if (j >= 0.20) {
      status <- "~ OK"
    } else {
      status <- "⚠ LOW"
      all_good <- FALSE
    }
    message(sprintf("  Wave %d → %d: %.3f %s", from_wave, to_wave, j, status))
  }
  
  if (all_good) {
    message("\n✓ Network stability looks good for SAOM convergence!")
  } else {
    message("\n⚠ Some Jaccard indices are low - SAOM may have convergence issues")
  }
  
  # Save outputs
  save(list_by_wave, file = file.path(output_dir, "list_by_wave.RData"))
  write_participants(list_by_wave[[1]], output_dir)
  write_outcomes(list_by_wave[[1]], output_dir)
  
  schema <- capture_schema(list_by_wave)
  utils::write.csv(schema, file.path(output_dir, "list_by_wave_schema.csv"), row.names = FALSE)
  
  # Write generation report with Jaccard indices
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    write_generation_report(list_by_wave, output_dir, jaccard_indices)
  }
  
  # Create marker file indicating realistic proxy data
  marker_file <- file.path(output_dir, ".realistic_proxy_data")
  writeLines(sprintf("Generated: %s\nN=%d, Waves=%d", 
                     format(Sys.time()), CONFIG$n_participants, CONFIG$n_waves), 
             marker_file)
  
  # Remove old synthetic marker if present
  old_marker <- file.path(output_dir, ".chapter4_synthetic")
  if (file.exists(old_marker)) {
    file.remove(old_marker)
  }
  
  message(sprintf("\n=== Realistic proxy data staged at %s ===", output_dir))
  message("Run 'make chapter4' to rebuild Chapter 4 outputs with new data")
}

# ==============================================================================
# Entry Point
# ==============================================================================

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error staging realistic proxy data: ", e$message)
      quit(status = 1)
    }
  )
}
