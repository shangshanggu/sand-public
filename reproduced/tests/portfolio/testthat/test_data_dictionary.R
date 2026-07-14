# ---------------------------------------------------------------------------
# Property tests for the Data Dictionary Viewer (Properties 10-11)
#
# Feature: portfolio-packaging
# Validates: Requirements 5.1, 5.3, 5.4
# ---------------------------------------------------------------------------

library(testthat)

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

.load_dictionary_functions <- function() {
  script_path <- file.path(REPRO_ROOT, "scripts", "portfolio",
                           "generate_data_dictionary.R")
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
    is_required_value <- grepl(
      "^(variable_descriptions|derivation_formulas)\\s*(<-|=)\\s*list\\(",
      first_line
    )

    if (is_fn_assign || is_operator || is_required_value) {
      eval(expr, envir = env)
    }
  }

  env
}

DICT_ENV <- .load_dictionary_functions()

.load_proxy_list_by_wave <- function() {
  data_path <- file.path(REPRO_ROOT, "data", "proxy", "list_by_wave.RData")
  stopifnot(file.exists(data_path))
  e <- new.env(parent = emptyenv())
  load(data_path, envir = e)
  stopifnot(exists("list_by_wave", envir = e, inherits = FALSE))
  get("list_by_wave", envir = e, inherits = FALSE)
}

.build_expected_wave_map <- function(list_by_wave) {
  all_vars <- sort(unique(unlist(lapply(list_by_wave, colnames), use.names = FALSE)))
  out <- setNames(vector("list", length(all_vars)), all_vars)
  for (vn in all_vars) {
    waves <- which(vapply(list_by_wave, function(df) vn %in% colnames(df), logical(1)))
    out[[vn]] <- paste(waves, collapse = ", ")
  }
  out
}

.render_dictionary_lines <- function(list_by_wave) {
  DICT_ENV$n_waves <- length(list_by_wave)
  DICT_ENV$data_mode <- "proxy"
  inventory <- DICT_ENV$build_variable_inventory(list_by_wave)
  lines <- DICT_ENV$render_data_dictionary(inventory)
  list(lines = lines, inventory = inventory)
}


# ===========================================================================
# Property 10: Data dictionary completeness (all variables with wave presence)
# Feature: portfolio-packaging, Property 10: Data dictionary completeness
# **Validates: Requirements 5.1, 5.4**
# ===========================================================================

test_that("Property 10: every variable appears in the dictionary with wave presence", {
  list_by_wave <- .load_proxy_list_by_wave()
  rendered <- .render_dictionary_lines(list_by_wave)
  lines <- rendered$lines

  expected_waves <- .build_expected_wave_map(list_by_wave)
  all_vars <- names(expected_waves)
  expect_true(length(all_vars) > 0)

  for (vn in all_vars) {
    escaped_var <- gsub("([][{}()+*^$.|\\\\?])", "\\\\\\1", vn)
    row_pattern <- paste0("^\\| `", escaped_var, "` \\|")
    row <- grep(row_pattern, lines, value = TRUE)
    expect_equal(
      length(row), 1L,
      info = paste("Variable must appear exactly once:", vn)
    )

    wave_pat <- paste0("\\|\\s*", expected_waves[[vn]], "\\s*\\|\\s*$")
    expect_true(
      grepl(wave_pat, row[[1]], perl = TRUE),
      info = paste("Wave presence must match source data for variable:", vn)
    )
  }
})

test_that("Property 10: key variable categories are present", {
  list_by_wave <- .load_proxy_list_by_wave()
  rendered <- .render_dictionary_lines(list_by_wave)
  lines <- rendered$lines

  required_sections <- c(
    "## Identifiers",
    "## Demographics",
    "## AUDIT-C / Alcohol Measures",
    "## Friendship Nominations",
    "## Norm Perceptions (Self & Friend-Level)",
    "## Derived Measures"
  )

  for (section in required_sections) {
    expect_true(section %in% lines, info = paste("Missing section:", section))
  }
})

test_that("proxy identifiers are labelled synthetic rather than anonymised", {
  list_by_wave <- .load_proxy_list_by_wave()
  rendered <- .render_dictionary_lines(list_by_wave)
  text <- paste(rendered$lines, collapse = "\n")

  expect_true(grepl("synthetic display labels", text, fixed = TRUE))
  expect_false(grepl("anonymised ID format", text, fixed = TRUE))
})


# ===========================================================================
# Property 11: Derived measures include derivation metadata
# Feature: portfolio-packaging, Property 11: derivation for derived measures
# **Validates: Requirements 5.3**
# ===========================================================================

test_that("Property 11: every derived variable has a derivation entry", {
  list_by_wave <- .load_proxy_list_by_wave()
  rendered <- .render_dictionary_lines(list_by_wave)
  lines <- rendered$lines
  inventory <- rendered$inventory

  derived_vars <- names(inventory)[vapply(
    names(inventory),
    function(vn) DICT_ENV$categorise_variable(vn) == "derived",
    logical(1)
  )]
  expect_true(length(derived_vars) > 0)

  for (vn in derived_vars) {
    escaped_var <- gsub("([][{}()+*^$.|\\\\?])", "\\\\\\1", vn)
    deriv_pattern <- paste0("^- \\*\\*`", escaped_var, "`\\*\\*:")
    matches <- grep(deriv_pattern, lines, value = TRUE)
    expect_equal(
      length(matches), 1L,
      info = paste("Derived variable must have derivation metadata:", vn)
    )
    expect_true(nchar(trimws(matches[[1]])) > 10)
  }
})

test_that("Property 11: deno_audit_peer is treated as a derived variable", {
  expect_equal(DICT_ENV$categorise_variable("deno_audit_peer"), "derived")
})
