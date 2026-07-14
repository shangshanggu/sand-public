#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# generate_data_dictionary.R
#
# Reads proxy (or real) list_by_wave.RData, inspects column structure across
# all waves, categorises variables, and produces a self-contained Markdown
# data dictionary at outputs/portfolio/data_dictionary.md.
#
# Usage (from reproduced/):
#   Rscript scripts/portfolio/generate_data_dictionary.R
# ---------------------------------------------------------------------------

# ---- bootstrap: locate repo root and load shared helpers ------------------

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches  <- grep(file_arg, cmd_args)
  if (length(matches) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", cmd_args[matches[1]]),
                                 winslash = "/", mustWork = TRUE)))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile,
                                 winslash = "/", mustWork = TRUE)))
  }
  getwd()
}

script_dir   <- get_script_dir()
loader_path  <- file.path(script_dir, "..", "utils", "config_loader.R")

if (file.exists(loader_path)) source(loader_path)

# ---- load configuration and resolve data directory ------------------------

config_candidates <- c("config/thesis.yml", "reproduced/config/thesis.yml")
config_path <- NULL
for (cand in config_candidates) {
  if (file.exists(cand)) { config_path <- cand; break }
}
if (is.null(config_path)) {
  stop("thesis.yml not found. Cannot generate data dictionary.")
}

bundle    <- load_configuration(config_path)
repo_root <- bundle$repo_root
repro_root <- file.path(repo_root, "reproduced")

# Resolve data mode (proxy by default for portfolio outputs)
data_mode <- get_config_value(bundle, "data", "mode", default = "real")
env_mode  <- Sys.getenv("SAND_DATA_MODE", "")
if (nzchar(env_mode)) data_mode <- env_mode

data_dir <- resolve_data_dir(bundle, mode_override = data_mode)
data_file <- file.path(data_dir, "list_by_wave.RData")

if (!file.exists(data_file)) {
  stop(sprintf("list_by_wave.RData not found at %s. Stage data first.", data_file))
}

output_dir  <- file.path(repro_root, "outputs", "portfolio")
output_file <- file.path(output_dir, "data_dictionary.md")

# ---- load data ------------------------------------------------------------

load(data_file)
if (!exists("list_by_wave") || !is.list(list_by_wave)) {
  stop("list_by_wave.RData must contain a list named 'list_by_wave'.")
}

n_waves <- length(list_by_wave)
message(sprintf("[data-dictionary] Loaded %d waves from %s", n_waves, data_file))

# ---- variable categorisation rules ----------------------------------------

categorise_variable <- function(varname) {
  if (varname %in% c("redcap_survey_identifier", "redcap_event_name")) {
    return("identifiers")
  }
  if (varname %in% c("age", "sex", "ethnicity", "majority_status",
                      "residence_cluster", "number_block", "number_flat")) {
    return("demographics")
  }
  if (varname %in% c("q1", "q2", "q3", "audit_score", "byaacq_6")) {
    return("audit_c")
  }
  if (varname %in% c("nomination", "which_friendid", "friend_number")) {
    return("nominations")
  }
  # Derived measures: actual_*_peer, misperception_*_peer, deno*_peer, inno*_peer
  if (grepl("^actual_.*_peer$", varname)) return("derived")
  if (grepl("^misperception_", varname))  return("derived")
  if (varname == "deno_audit_peer") return("derived")
  if (grepl("^(deno|inno)\\d*_peer$", varname)) return("derived")
  # Norm perceptions: inno*_self, deno*_friend_*, inno*_friend_*
  if (grepl("^inno\\d+_self$", varname))    return("norm_perceptions")
  if (grepl("^(deno|inno)\\d+_friend_", varname)) return("norm_perceptions")
  # Fallback
  "other"
}

# ---- variable descriptions ------------------------------------------------

variable_descriptions <- list(
  redcap_survey_identifier = "Synthetic actor identifier in proxy outputs; the protected workflow uses a pseudonymised study identifier",
  redcap_event_name        = "REDCap event label indicating the survey wave (e.g., wave1_arm_1)",
  age                      = "Participant age in years at time of survey",
  sex                      = "Biological sex (0 = female, 1 = male)",
  ethnicity                = "Self-reported ethnicity category (1\u20134)",
  majority_status          = "Composite majority-status covariate used in Chapter 7 where available",
  residence_cluster        = "Synthetic residence grouping used to generate proxy homophily; identical to the synthetic block assignment",
  number_block             = "Synthetic residence-block assignment used to exercise the Chapter 7 blockmate covariate",
  number_flat              = "Synthetic flat-within-block assignment used to exercise the Chapter 7 flatmate covariate",
  q1                       = "AUDIT-C item 1: frequency of drinking (0 = never, 4 = 4+ times/week)",
  q2                       = "AUDIT-C item 2: typical number of drinks per occasion (0 = 1\u20132, 4 = 10+)",
  q3                       = "AUDIT-C item 3: frequency of binge drinking / 6+ drinks (0 = never, 4 = daily)",
  audit_score              = "AUDIT-C composite score (sum of q1 + q2 + q3, range 0\u201312)",
  byaacq_6                 = "BYAACQ-derived passing-out field used to construct the Chapter 6 alcohol-induced blackout outcome",
  nomination               = "Identifier of the nominated important peer (participant ID)",
  which_friendid           = "Nomination slot index (1\u201310, indicating which important-peer nomination this row represents)",
  friend_number            = "Total number of important-peer nominations made by this participant",
  inno1_self               = "Self-reported approval of not drinking in social settings",
  inno2_self               = "Self-reported approval of binge drinking",
  inno3_self               = "Self-reported approval of drinking enough to pass out"
)

# Pattern-based descriptions for friend-level norm perception columns
describe_friend_norm <- function(varname) {
  m <- regmatches(varname, regexec("^(deno|inno)(\\d+)_friend_(\\d+)$", varname))[[1]]
  if (length(m) == 0) return(NULL)
  norm_type <- m[2]
  norm_num  <- m[3]
  friend_idx <- m[4]

  type_label <- if (norm_type == "deno") "Descriptive" else "Injunctive"

  norm_descs <- list(
    deno1 = "drinking frequency",
    deno3 = "binge drinking frequency",
    deno4 = "getting drunk frequency",
    inno1 = "approval of not drinking",
    inno2 = "approval of binge drinking",
    inno3 = "approval of drinking enough to pass out"
  )
  key <- paste0(norm_type, norm_num)
  measure <- norm_descs[[key]]
  if (is.null(measure)) measure <- paste0("norm item ", norm_num)

  if (friend_idx == "0") {
    return(sprintf("%s norm perception of %s for a typical resident (global reference)", type_label, measure))
  }
  sprintf("%s norm perception of %s attributed to nominated important peer %s", type_label, measure, friend_idx)
}

# Descriptions for derived measures
describe_derived <- function(varname) {
  if (grepl("^actual_audit_score_peer$", varname))
    return("Mean AUDIT-C score of nominated important peers")
  if (grepl("^actual_(deno|inno)(\\d+)_peer$", varname)) {
    m <- regmatches(varname, regexec("^actual_(deno|inno)(\\d+)_peer$", varname))[[1]]
    item_labels <- list(
      deno1 = "descriptive drinking-frequency item",
      deno3 = "descriptive heavy-episodic-drinking item",
      deno4 = "descriptive drunkenness item",
      inno1 = "approval of not drinking",
      inno2 = "approval of binge drinking",
      inno3 = "approval of drinking enough to pass out"
    )
    key <- paste0(m[2], m[3])
    item_label <- item_labels[[key]]
    if (is.null(item_label)) item_label <- sprintf("%s norm item %s", m[2], m[3])
    return(sprintf("Mean actual %s across nominated important peers", item_label))
  }
  if (grepl("^misperception_audit_score_peer$", varname))
    return("Misperception of important-peer AUDIT-C score (perceived minus actual peer mean)")
  if (grepl("^misperception_(q\\d+)_peer$", varname)) {
    m <- regmatches(varname, regexec("^misperception_(q\\d+)_peer$", varname))[[1]]
    return(sprintf("Misperception of important-peer %s score (perceived minus actual peer mean)", toupper(m[2])))
  }
  if (grepl("^misperception_(inno\\d+)_peer$", varname)) {
    m <- regmatches(varname, regexec("^misperception_(inno\\d+)_peer$", varname))[[1]]
    item_labels <- list(
      inno1 = "approval of not drinking",
      inno2 = "approval of binge drinking",
      inno3 = "approval of drinking enough to pass out"
    )
    item_label <- item_labels[[m[2]]]
    if (is.null(item_label)) item_label <- m[2]
    return(sprintf("Misperception of important-peer %s (perceived minus actual peer mean)", item_label))
  }
  if (grepl("^deno_audit_peer$", varname))
    return("Perceived important-peer AUDIT-C score (descriptive norm composite)")
  if (grepl("^(deno|inno)(\\d+)_peer$", varname)) {
    m <- regmatches(varname, regexec("^(deno|inno)(\\d+)_peer$", varname))[[1]]
    item_labels <- list(
      deno1 = "descriptive drinking-frequency item",
      deno3 = "descriptive heavy-episodic-drinking item",
      deno4 = "descriptive drunkenness item",
      inno1 = "approval of not drinking",
      inno2 = "approval of binge drinking",
      inno3 = "approval of drinking enough to pass out"
    )
    key <- paste0(m[2], m[3])
    item_label <- item_labels[[key]]
    if (is.null(item_label)) item_label <- sprintf("%s norm item %s", m[2], m[3])
    return(sprintf("Aggregated perceived important-peer %s", item_label))
  }
  "Derived measure"
}

# Derivation formulas for derived measures
derivation_formulas <- list(
  actual_audit_score_peer = "mean(audit_score) across nominated important peers",
  actual_deno1_peer       = "mean(deno1_friend_k) across nominated important peers, where k indexes each peer",
  actual_deno3_peer       = "mean(deno3_friend_k) across nominated important peers",
  actual_deno4_peer       = "mean(deno4_friend_k) across nominated important peers",
  actual_inno1_peer       = "mean(inno1_friend_k) across nominated important peers",
  actual_inno2_peer       = "mean(inno2_friend_k) across nominated important peers",
  actual_inno3_peer       = "mean(inno3_friend_k) across nominated important peers",
  misperception_audit_score_peer = "deno_audit_peer \u2212 actual_audit_score_peer",
  misperception_q1_peer   = "perceived important-peer q1 \u2212 actual important-peer mean q1",
  misperception_q2_peer   = "perceived important-peer q2 \u2212 actual important-peer mean q2",
  misperception_q3_peer   = "perceived important-peer q3 \u2212 actual important-peer mean q3",
  misperception_inno1_peer = "perceived important-peer inno1 \u2212 actual important-peer mean inno1",
  misperception_inno2_peer = "perceived important-peer inno2 \u2212 actual important-peer mean inno2",
  misperception_inno3_peer = "perceived important-peer inno3 \u2212 actual important-peer mean inno3",
  deno_audit_peer         = "Aggregated descriptive norm perception of important-peer AUDIT-C score",
  deno1_peer              = "Aggregated descriptive norm perception of important-peer drinking frequency",
  deno3_peer              = "Aggregated descriptive norm perception of important-peer binge drinking",
  deno4_peer              = "Aggregated descriptive norm perception of important-peer drunkenness",
  inno1_peer              = "Aggregated injunctive norm perception of important-peer approval of not drinking",
  inno2_peer              = "Aggregated injunctive norm perception of important-peer approval of binge drinking",
  inno3_peer              = "Aggregated injunctive norm perception of important-peer approval of drinking enough to pass out"
)


# ---- inspect all columns across waves -------------------------------------

build_variable_inventory <- function(list_by_wave) {
  all_vars <- list()

  for (wave_idx in seq_along(list_by_wave)) {
    df <- list_by_wave[[wave_idx]]
    for (cn in colnames(df)) {
      col <- df[[cn]]
      if (is.null(all_vars[[cn]])) {
        all_vars[[cn]] <- list(
          name       = cn,
          r_classes  = character(0),
          min_vals   = numeric(0),
          max_vals   = numeric(0),
          waves      = integer(0),
          has_na     = FALSE,
          is_numeric = is.numeric(col) || is.integer(col),
          is_char    = is.character(col) || is.factor(col),
          unique_char_vals = character(0)
        )
      }
      entry <- all_vars[[cn]]
      entry$waves <- c(entry$waves, as.integer(wave_idx))
      entry$r_classes <- unique(c(entry$r_classes, class(col)))
      if (any(is.na(col))) entry$has_na <- TRUE
      if (is.numeric(col) || is.integer(col)) {
        rng <- suppressWarnings(range(col, na.rm = TRUE))
        if (all(is.finite(rng))) {
          entry$min_vals <- c(entry$min_vals, rng[1])
          entry$max_vals <- c(entry$max_vals, rng[2])
        }
      }
      if (is.character(col) || is.factor(col)) {
        entry$unique_char_vals <- unique(c(entry$unique_char_vals, unique(as.character(col))))
      }
      all_vars[[cn]] <- entry
    }
  }
  all_vars
}

format_type <- function(entry) {
  paste(entry$r_classes, collapse = "/")
}

format_range <- function(entry, varname = NULL) {
  # Suppress identifier ranges without mislabelling pseudonymised data as anonymous.
  if (!is.null(varname) && varname %in% c("redcap_survey_identifier", "nomination")) {
    if (identical(data_mode, "proxy")) {
      return("P001–P### (synthetic display labels)")
    }
    return("Pseudonymised study identifiers (values suppressed)")
  }
  if (entry$is_char) {
    n <- length(entry$unique_char_vals)
    if (n <= 6) {
      return(paste(entry$unique_char_vals, collapse = ", "))
    }
    return(sprintf("%d unique values", n))
  }
  if (length(entry$min_vals) == 0) return("N/A")
  overall_min <- min(entry$min_vals)
  overall_max <- max(entry$max_vals)
  if (overall_min == overall_max) return(as.character(overall_min))
  sprintf("%s\u2013%s", format(overall_min, nsmall = 0), format(overall_max, nsmall = 0))
}

format_waves <- function(entry) {
  paste(sort(unique(entry$waves)), collapse = ", ")
}

get_description <- function(varname, category) {
  # Check static descriptions first
  if (!is.null(variable_descriptions[[varname]])) {
    return(variable_descriptions[[varname]])
  }
  # Pattern-based friend norm descriptions
  if (grepl("^(deno|inno)\\d+_friend_\\d+$", varname)) {
    desc <- describe_friend_norm(varname)
    if (!is.null(desc)) return(desc)
  }
  # Derived measure descriptions
  if (category == "derived") {
    return(describe_derived(varname))
  }
  # Fallback
  varname
}

get_derivation <- function(varname, category = NULL) {
  if (!is.null(category) && category != "derived") {
    return(NULL)
  }
  derivation <- derivation_formulas[[varname]]
  if (!is.null(derivation) && nzchar(derivation)) {
    return(derivation)
  }
  if (!is.null(category) && category == "derived") {
    return("See reproduced/analyses/chapter4_data_collection/scripts/01_prepare_norm_data.R")
  }
  NULL
}

# ---- render Markdown document ---------------------------------------------

render_data_dictionary <- function(inventory) {
  # Categorise all variables
  entries <- lapply(names(inventory), function(vn) {
    entry <- inventory[[vn]]
    cat_name <- categorise_variable(vn)
    list(
      name        = vn,
      category    = cat_name,
      type        = format_type(entry),
      range       = format_range(entry, vn),
      description = get_description(vn, cat_name),
      waves       = format_waves(entry),
      derivation  = get_derivation(vn, cat_name)
    )
  })

  # Group by category
  category_order <- c("identifiers", "demographics", "audit_c", "nominations",
                       "norm_perceptions", "derived", "other")
  category_labels <- c(
    identifiers      = "Identifiers",
    demographics     = "Demographics",
    audit_c          = "AUDIT-C / Alcohol Measures",
    nominations      = "Friendship Nominations",
    norm_perceptions = "Norm Perceptions (Self & Friend-Level)",
    derived          = "Derived Measures",
    other            = "Other"
  )

  groups <- list()
  for (e in entries) {
    cat_name <- e$category
    if (is.null(groups[[cat_name]])) groups[[cat_name]] <- list()
    groups[[cat_name]] <- c(groups[[cat_name]], list(e))
  }

  proxy_banner <- character(0)
  if (data_mode == "proxy") {
    proxy_banner <- c(
      "",
      "> **\u26a0\ufe0f PROXY DATA** \u2014 This dictionary was generated from synthetic proxy inputs.",
      "> Variable ranges and distributions may differ from real institutional data.",
      ""
    )
  }

  lines <- c(
    "# Data Dictionary",
    "",
    "Reference for all variables in the SAND longitudinal dataset (`list_by_wave.RData`).",
    sprintf("Generated from %s data on %s.", data_mode, format(Sys.time(), "%Y-%m-%d")),
    proxy_banner,
    "",
    "## Table of Contents",
    ""
  )

  # TOC
  for (cat_name in category_order) {
    if (!is.null(groups[[cat_name]])) {
      label <- category_labels[[cat_name]]
      anchor <- gsub("[^a-z0-9 ]", "", tolower(label))
      anchor <- gsub(" +", "-", anchor)
      lines <- c(lines, sprintf("- [%s](#%s) (%d variables)",
                                label, anchor, length(groups[[cat_name]])))
    }
  }
  lines <- c(lines, "")

  # Per-category sections
  for (cat_name in category_order) {
    grp <- groups[[cat_name]]
    if (is.null(grp)) next
    label <- category_labels[[cat_name]]
    lines <- c(lines,
      sprintf("## %s", label), "",
      "| Variable | Type | Range | Description | Waves |",
      "|----------|------|-------|-------------|-------|"
    )
    for (e in grp) {
      desc <- gsub("\\|", "\\\\|", e$description)
      lines <- c(lines, sprintf("| `%s` | %s | %s | %s | %s |",
                                e$name, e$type, e$range, desc, e$waves))
    }
    lines <- c(lines, "")
  }

  # Derivation formulas section
  derived_with_formulas <- Filter(function(e) !is.null(e$derivation), entries)
  if (length(derived_with_formulas) > 0) {
    lines <- c(lines,
      "## Derivation Formulas", "",
      "The following derived measures are computed from raw variables:", ""
    )
    for (e in derived_with_formulas) {
      lines <- c(lines, sprintf("- **`%s`**: %s", e$name, e$derivation))
    }
    lines <- c(lines, "",
      "All `actual_*_peer` measures are computed as the mean of the corresponding",
      "variable across a participant's nominated important peers. Misperception scores",
      "represent the difference between a participant's perception of those peers and",
      "the actual important-peer mean (perceived minus actual).", "")
  }

  # Summary
  total_vars <- length(entries)
  lines <- c(lines,
    "## Summary", "",
    sprintf("- **Total variables:** %d", total_vars),
    sprintf("- **Waves:** %d", n_waves),
    sprintf("- **Data mode:** %s", data_mode),
    ""
  )

  lines
}

# ---- main -----------------------------------------------------------------

main <- function() {
  message("[data-dictionary] Building variable inventory...")

  inventory <- build_variable_inventory(list_by_wave)
  message(sprintf("[data-dictionary] Found %d unique variables across %d waves",
                  length(inventory), n_waves))

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  md_lines <- render_data_dictionary(inventory)
  writeLines(md_lines, output_file, useBytes = TRUE)

  message(sprintf("[data-dictionary] Data dictionary written to %s", output_file))
  message(sprintf("[data-dictionary] %d variables documented", length(inventory)))
  invisible(inventory)
}

if (identical(environment(), globalenv()) && !length(sys.frames())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error: ", e$message)
      quit(status = 1)
    }
  )
}
