# ---------------------------------------------------------------------------
# Property tests for the Network Explorer enhancements (Properties 5–9)
#
# Feature: portfolio-packaging
# Validates: Requirements 4.1, 4.2, 4.5, 4.6, 4.7, 7.2
# ---------------------------------------------------------------------------

library(testthat)
library(igraph)
library(visNetwork)
library(htmlwidgets)
library(htmltools)

# ---- Resolve the real reproduced/ root ------------------------------------

.find_repro_root <- function() {
  if (exists("REPO_ROOT") &&
      file.exists(file.path(REPO_ROOT, "config", "thesis.yml"))) {
    return(REPO_ROOT)
  }
  d <- getwd()
  for (i in 1:10) {
    if (file.exists(file.path(d, "config", "thesis.yml"))) return(d)
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }
  stop("Cannot locate reproduced/ root")
}

REPRO_ROOT <- .find_repro_root()

# ---- Load Network Explorer functions via AST parsing ----------------------
#
# Strategy: same as test_validation_dashboard.R — parse the script, extract
# only function assignments, and evaluate them in a controlled environment
# that already has the shared helpers loaded.

.load_network_functions <- function() {
  # Source shared viz utilities first
  source(file.path(REPRO_ROOT, "scripts", "visualisation", "viz_utils.R"),
         local = globalenv())

  script_path <- file.path(REPRO_ROOT, "scripts", "visualisation",
                           "05_interactive_networks.R")
  stopifnot(file.exists(script_path))

  exprs <- parse(script_path)
  env <- new.env(parent = globalenv())

  for (expr in exprs) {
    expr_text <- deparse(expr, width.cutoff = 500L)
    first_line <- expr_text[1]

    is_fn_assign <- grepl(
      "^(`[^`]+`|[A-Za-z_.][A-Za-z0-9_.]*) *(<-|=) *function\\b",
      first_line
    )
    is_operator <- grepl("^`%", first_line) && grepl("<-", first_line)
    # Capture value assignments: constants, named vectors, design tokens
    is_value_assign <- grepl(
      "^[A-Za-z_.][A-Za-z0-9_.]*\\s*(<-|=)\\s*(c\\(|[\"'])",
      first_line
    )

    if (is_fn_assign || is_operator || is_value_assign) {
      eval(expr, envir = env)
    }
  }

  env
}

NET_ENV <- .load_network_functions()

# ---- Proxy data loader -----------------------------------------------------

.load_proxy_data <- function() {
  data_dir <- file.path(REPRO_ROOT, "data", "proxy")
  stopifnot(file.exists(file.path(data_dir, "list_by_wave.RData")))
  load_list_by_wave(data_dir)
}

# ---- Shared test helpers ---------------------------------------------------

SAOM_WAVES <- c(2L, 4L, 5L, 6L)

#' Build public-safe synthetic graphs for all SAOM waves from proxy data.
#' Returns a list with $graphs, $anon_ids, $anon_map.
build_test_graphs <- function(list_by_wave) {
  all_ids <- sort(unique(list_by_wave[[1]]$redcap_survey_identifier))
  anon_map <- setNames(
    sprintf("P%03d", seq_along(all_ids)),
    as.character(all_ids)
  )
  anon_ids <- unname(anon_map)

  graphs <- lapply(SAOM_WAVES, function(w) {
    wave_df <- list_by_wave[[w]]
    wave_df$redcap_survey_identifier <-
      anon_map[as.character(wave_df$redcap_survey_identifier)]
    if ("nomination" %in% names(wave_df)) {
      wave_df$nomination <- anon_map[as.character(wave_df$nomination)]
    }
    NET_ENV$build_wave_graph(wave_df, anon_ids)
  })

  list(graphs = graphs, anon_ids = anon_ids, anon_map = anon_map)
}

#' Generate all wave HTML files + combined HTML to a temp directory.
#' Returns the output directory path.
generate_html_to_tempdir <- function(list_by_wave, wave_captions = NULL) {
  result <- build_test_graphs(list_by_wave)
  output_dir <- tempfile(pattern = "net_test_")
  dir.create(output_dir, recursive = TRUE)

  wave_labels <- paste("Wave", SAOM_WAVES)
  wave_filenames <- character(length(SAOM_WAVES))

  for (i in seq_along(SAOM_WAVES)) {
    vn_data <- NET_ENV$build_visnetwork_data(
      result$graphs[[i]], wave_labels[i], data_mode = "proxy"
    )
    wave_num <- SAOM_WAVES[i]
    filename <- sprintf("network_wave%d.html", wave_num)
    wave_filenames[i] <- filename
    out_file <- file.path(output_dir, filename)

    wave_stats <- NET_ENV$compute_wave_stats(result$graphs[[i]])

    wn_key <- as.character(wave_num)
    wave_caption <- if (!is.null(wave_captions) &&
                        !is.null(wave_captions[[wn_key]]) &&
                        nzchar(wave_captions[[wn_key]])) {
      wave_captions[[wn_key]]
    } else {
      "Caption not configured for this wave."
    }

    NET_ENV$render_wave_html(vn_data$nodes_df, vn_data$edges_df,
                             wave_labels[i], wave_num, out_file,
                             physics = FALSE,
                             stats = wave_stats, caption = wave_caption,
                             data_mode = "proxy")
  }

  NET_ENV$render_combined_html(wave_filenames, wave_labels, SAOM_WAVES,
                               file.path(output_dir, "network_all_waves.html"),
                               captions = wave_captions,
                               data_mode = "proxy")

  output_dir
}


# ===========================================================================
# Property 5: Network Explorer produces one HTML file per SAOM wave
# Feature: portfolio-packaging, Property 5: Network Explorer produces one
#   HTML file per SAOM wave
# **Validates: Requirements 4.1**
# ===========================================================================

test_that("Property 5: exactly 4 per-wave HTML files + 1 combined HTML are produced", {
  lbw <- .load_proxy_data()
  cfg <- yaml::read_yaml(file.path(REPRO_ROOT, "config", "thesis.yml"))
  captions <- cfg$visualisation$interactive_network$wave_captions

  output_dir <- generate_html_to_tempdir(lbw, wave_captions = captions)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  expected_files <- c(
    "network_wave2.html",
    "network_wave4.html",
    "network_wave5.html",
    "network_wave6.html",
    "network_all_waves.html"
  )

  actual_files <- list.files(output_dir, pattern = "\\.html$")

  for (f in expected_files) {
    expect_true(f %in% actual_files,
                info = paste("Expected file missing:", f))
  }
  expect_equal(length(actual_files), 5L,
               info = "Should produce exactly 5 HTML files")
})

test_that("Property 5: each per-wave HTML file is non-empty", {
  lbw <- .load_proxy_data()
  output_dir <- generate_html_to_tempdir(lbw)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  for (w in SAOM_WAVES) {
    f <- file.path(output_dir, sprintf("network_wave%d.html", w))
    expect_true(file.exists(f), info = paste("File must exist:", basename(f)))
    expect_true(file.info(f)$size > 100,
                info = paste("File must be non-trivial:", basename(f)))
  }
})

test_that("Property 5: proxy HTML carries visible provenance labels", {
  lbw <- .load_proxy_data()
  output_dir <- generate_html_to_tempdir(lbw)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  expected_files <- c(
    sprintf("network_wave%d.html", SAOM_WAVES),
    "network_all_waves.html"
  )
  for (filename in expected_files) {
    html <- paste(
      readLines(file.path(output_dir, filename), warn = FALSE),
      collapse = "\n"
    )
    expect_true(
      grepl("SYNTHETIC PROXY DATA", html, fixed = TRUE),
      info = paste(filename, "must visibly identify synthetic proxy data")
    )
    expect_true(
      grepl("data-sand-data-mode=['\"]proxy['\"]", html),
      info = paste(filename, "must embed machine-readable proxy provenance")
    )
    expect_false(
      grepl("PROTECTED REAL DATA", html, fixed = TRUE),
      info = paste(filename, "must not be marked as a real-data rendering")
    )
  }
})


# ===========================================================================
# Property 6: Network Explorer tooltips contain all required fields
# Feature: portfolio-packaging, Property 6: Network Explorer tooltips
#   contain all required fields
# **Validates: Requirements 4.2**
# ===========================================================================

test_that("Property 6: tooltip HTML contains participant ID, AUDIT-C, in-degree, out-degree", {
  lbw <- .load_proxy_data()
  result <- build_test_graphs(lbw)

  # Test on wave 2 (index 1)
  vn_data <- NET_ENV$build_visnetwork_data(
    result$graphs[[1]], "Wave 2", data_mode = "proxy"
  )
  nodes_df <- vn_data$nodes_df

  expect_true("title" %in% names(nodes_df),
              info = "nodes_df must have a 'title' column for tooltips")
  expect_true(nrow(nodes_df) > 0,
              info = "Must have at least one node")

  # Check a sample of tooltips
  n_check <- min(20, nrow(nodes_df))
  for (i in seq_len(n_check)) {
    tooltip <- nodes_df$title[i]
    node_id <- nodes_df$id[i]

    # Synthetic actor ID in P### format
    expect_true(grepl("Synthetic actor P\\d{3}", tooltip),
                info = paste("Tooltip for", node_id,
                             "must identify a synthetic actor (P### format)"))

    # AUDIT-C field
    expect_true(grepl("AUDIT-C", tooltip),
                info = paste("Tooltip for", node_id,
                             "must contain AUDIT-C"))

    # In-degree and out-degree (shown as "in X, out Y" in the Degree row)
    expect_true(grepl("\\bin \\d+", tooltip),
                info = paste("Tooltip for", node_id,
                             "must contain in-degree"))
    expect_true(grepl("\\bout \\d+", tooltip),
                info = paste("Tooltip for", node_id,
                             "must contain out-degree"))
  }
})

test_that("Property 6: tooltips use P### format IDs across all waves", {
  lbw <- .load_proxy_data()
  result <- build_test_graphs(lbw)

  for (i in seq_along(SAOM_WAVES)) {
    vn_data <- NET_ENV$build_visnetwork_data(
      result$graphs[[i]], paste("Wave", SAOM_WAVES[i]), data_mode = "proxy"
    )
    tooltips <- vn_data$nodes_df$title

    # Every tooltip should reference a P### ID
    has_anon_id <- grepl("Synthetic actor P\\d{3}", tooltips)
    expect_true(all(has_anon_id),
                info = paste("All tooltips in Wave", SAOM_WAVES[i],
                             "must use P### format IDs"))
  }
})


# ===========================================================================
# Property 7: Network Explorer includes summary statistics per wave
# Feature: portfolio-packaging, Property 7: Network Explorer includes
#   summary statistics per wave
# **Validates: Requirements 4.5**
# ===========================================================================

test_that("Property 7: wave HTML contains Network Summary with required stats", {
  lbw <- .load_proxy_data()
  output_dir <- generate_html_to_tempdir(lbw)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  required_labels <- c("Network Summary", "Density", "Reciprocity",
                        "Avg In-Degree", "Synthetic Actors")

  for (w in SAOM_WAVES) {
    html_file <- file.path(output_dir, sprintf("network_wave%d.html", w))
    html_content <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

    for (label in required_labels) {
      expect_true(grepl(label, html_content, fixed = TRUE),
                  info = paste("Wave", w, "HTML must contain:", label))
    }
  }
})

test_that("Property 7: compute_wave_stats returns all required fields", {
  lbw <- .load_proxy_data()
  result <- build_test_graphs(lbw)

  required_fields <- c("density", "reciprocity", "avg_in_degree",
                        "avg_out_degree", "n_participants", "n_nominations")

  for (i in seq_along(SAOM_WAVES)) {
    stats <- NET_ENV$compute_wave_stats(result$graphs[[i]])

    for (field in required_fields) {
      expect_true(field %in% names(stats),
                  info = paste("Wave", SAOM_WAVES[i],
                               "stats must include:", field))
      expect_true(is.numeric(stats[[field]]),
                  info = paste("Wave", SAOM_WAVES[i], field,
                               "must be numeric"))
    }

    # Density and reciprocity should be in [0, 1]
    expect_true(stats$density >= 0 && stats$density <= 1,
                info = paste("Wave", SAOM_WAVES[i],
                             "density must be in [0,1]"))
    expect_true(stats$reciprocity >= 0 && stats$reciprocity <= 1,
                info = paste("Wave", SAOM_WAVES[i],
                             "reciprocity must be in [0,1]"))
  }
})


# ===========================================================================
# Property 8: All generated outputs contain only synthetic display identifiers
# Feature: portfolio-packaging, Property 8: All generated outputs contain
#   only synthetic display identifiers
# **Validates: Requirements 4.6, 7.2**
# ===========================================================================

test_that("Property 8: HTML files contain no real participant IDs", {
  lbw <- .load_proxy_data()

  # Collect real IDs from the proxy data
  real_ids <- sort(unique(lbw[[1]]$redcap_survey_identifier))
  # These are numeric IDs like 1001, 1002, etc.
  expect_true(length(real_ids) > 0, info = "Must have real IDs to check against")

  cfg <- yaml::read_yaml(file.path(REPRO_ROOT, "config", "thesis.yml"))
  captions <- cfg$visualisation$interactive_network$wave_captions

  output_dir <- generate_html_to_tempdir(lbw, wave_captions = captions)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  html_files <- list.files(output_dir, pattern = "\\.html$", full.names = TRUE)
  expect_true(length(html_files) > 0)

  for (html_file in html_files) {
    content <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

    # Check that none of the real numeric IDs appear as standalone numbers
    # (not as part of CSS values, pixel sizes, etc.)
    # We look for the ID preceded by a word boundary or common delimiters
    for (rid in real_ids) {
      rid_str <- as.character(rid)
      # Pattern: the real ID appearing as a participant reference
      # (preceded by "Participant " or as a node id in quotes)
      participant_pattern <- paste0("Participant\\s+", rid_str, "\\b")
      expect_false(grepl(participant_pattern, content),
                   info = paste("File", basename(html_file),
                                "must not contain real ID:", rid_str))
    }
  }
})

test_that("Property 8: all node IDs in visnetwork data use P### format", {
  lbw <- .load_proxy_data()
  result <- build_test_graphs(lbw)

  for (i in seq_along(SAOM_WAVES)) {
    vn_data <- NET_ENV$build_visnetwork_data(
      result$graphs[[i]], paste("Wave", SAOM_WAVES[i]), data_mode = "proxy"
    )
    node_ids <- vn_data$nodes_df$id

    # All IDs must match P### pattern
    expect_true(all(grepl("^P\\d{3}$", node_ids)),
                info = paste("Wave", SAOM_WAVES[i],
                             "all node IDs must be P### format"))

    # Edge from/to must also be P### format
    if (nrow(vn_data$edges_df) > 0) {
      expect_true(all(grepl("^P\\d{3}$", vn_data$edges_df$from)),
                  info = paste("Wave", SAOM_WAVES[i],
                               "all edge 'from' must be P### format"))
      expect_true(all(grepl("^P\\d{3}$", vn_data$edges_df$to)),
                  info = paste("Wave", SAOM_WAVES[i],
                               "all edge 'to' must be P### format"))
    }
  }
})


# ===========================================================================
# Property 9: Network Explorer includes interpretive caption per wave
# Feature: portfolio-packaging, Property 9: Network Explorer includes
#   interpretive caption per wave
# **Validates: Requirements 4.7**
# ===========================================================================

test_that("Property 9: wave HTML contains configured caption text", {
  lbw <- .load_proxy_data()
  cfg <- yaml::read_yaml(file.path(REPRO_ROOT, "config", "thesis.yml"))
  captions <- cfg$visualisation$interactive_network$wave_captions

  output_dir <- generate_html_to_tempdir(lbw, wave_captions = captions)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  for (w in SAOM_WAVES) {
    wn_key <- as.character(w)
    expected_caption <- captions[[wn_key]]
    expect_true(!is.null(expected_caption) && nzchar(expected_caption),
                info = paste("Config must have caption for wave", w))

    html_file <- file.path(output_dir, sprintf("network_wave%d.html", w))
    html_content <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

    # The caption text should appear in the HTML
    # Use a substring to avoid HTML entity issues
    caption_snippet <- substr(expected_caption, 1, min(40, nchar(expected_caption)))
    expect_true(grepl(caption_snippet, html_content, fixed = TRUE),
                info = paste("Wave", w, "HTML must contain caption text"))
  }
})

test_that("Property 9: default caption appears when no caption configured", {
  lbw <- .load_proxy_data()

  # Generate with NULL captions — should get default text
  output_dir <- generate_html_to_tempdir(lbw, wave_captions = NULL)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  for (w in SAOM_WAVES) {
    html_file <- file.path(output_dir, sprintf("network_wave%d.html", w))
    html_content <- paste(readLines(html_file, warn = FALSE), collapse = "\n")

    expect_true(grepl("Caption not configured for this wave", html_content,
                      fixed = TRUE),
                info = paste("Wave", w,
                             "must show default caption when none configured"))
  }
})

test_that("Property 9: combined HTML includes captions for each wave tab", {
  lbw <- .load_proxy_data()
  cfg <- yaml::read_yaml(file.path(REPRO_ROOT, "config", "thesis.yml"))
  captions <- cfg$visualisation$interactive_network$wave_captions

  output_dir <- generate_html_to_tempdir(lbw, wave_captions = captions)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  combined_file <- file.path(output_dir, "network_all_waves.html")
  html_content <- paste(readLines(combined_file, warn = FALSE), collapse = "\n")

  for (w in SAOM_WAVES) {
    wn_key <- as.character(w)
    expected_caption <- captions[[wn_key]]
    caption_snippet <- substr(expected_caption, 1, min(40, nchar(expected_caption)))
    expect_true(grepl(caption_snippet, html_content, fixed = TRUE),
                info = paste("Combined HTML must contain caption for wave", w))
  }
})
