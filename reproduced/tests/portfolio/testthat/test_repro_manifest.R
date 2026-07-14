# ---------------------------------------------------------------------------
# Property tests for the Reproducibility Manifest (Property 13, plus 16)
#
# Feature: portfolio-packaging
# Validates: Requirements 8.1, 8.2
# ---------------------------------------------------------------------------

library(testthat)
library(jsonlite)
library(yaml)

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
MANIFEST_SCRIPT <- file.path(REPRO_ROOT, "scripts", "portfolio", "generate_repro_manifest.R")
stopifnot(file.exists(MANIFEST_SCRIPT))

.load_manifest_env <- function() {
  env <- new.env(parent = globalenv())
  source(MANIFEST_SCRIPT, local = env)
  env
}

MANIFEST_ENV <- .load_manifest_env()

.write_file <- function(path, contents) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(contents, path)
}

.create_manifest_fixture <- function() {
  base <- tempfile(pattern = "repro_manifest_")
  repro <- file.path(base, "reproduced")
  dir.create(file.path(repro, "config"), recursive = TRUE, showWarnings = FALSE)

  cfg <- list(
    project = list(
      name = "fixture",
      version = "0.0.1",
      environment = list(conda_env = "r_stable"),
      paths = list(raw_data_dir = "reproduced/data/raw")
    ),
    data = list(
      mode = "proxy",
      proxy_dir = "reproduced/data/proxy"
    ),
    chapters = list(
      chapter4_data_collection = list(
        outputs_dir = "reproduced/outputs/chapter4",
        required_inputs = c("reproduced/data/raw/ch4_input.csv")
      ),
      chapter5_descriptive_norms = list(
        outputs_dir = "reproduced/outputs/chapter5",
        required_inputs = c("reproduced/outputs/chapter4/data/ch4_out.rds"),
        nam = list(seed = 20250921)
      ),
      chapter6_injunctive_norms = list(
        outputs_dir = "reproduced/outputs/chapter6",
        required_inputs = c("reproduced/outputs/chapter4/data/ch4_out.rds")
      ),
      chapter7_saom = list(
        outputs_dir = "reproduced/outputs/chapter7",
        required_inputs = c("reproduced/outputs/chapter4/data/ch4_out.rds")
      )
    ),
    rsiena = list(
      project_seed = 2022,
      estimation = list(seed = 2022)
    )
  )
  yaml::write_yaml(cfg, file.path(repro, "config", "thesis.yml"))

  # Inputs
  .write_file(file.path(repro, "data", "raw", "ch4_input.csv"), "id,value\n1,10")

  # Outputs + logs for each chapter
  for (ch in c("chapter4", "chapter5", "chapter6", "chapter7")) {
    .write_file(file.path(repro, "outputs", ch, "tables", "out.csv"), "metric,value\na,1")
    .write_file(
      file.path(repro, "outputs", ch, "logs", "run.json"),
      sprintf("{\"finished_at\":\"2026-02-12T12:00:0%dZ\"}", match(ch, c("chapter4", "chapter5", "chapter6", "chapter7")))
    )
  }

  # Upstream file used as input by chapter 5-7
  upstream_rds <- file.path(repro, "outputs", "chapter4", "data", "ch4_out.rds")
  dir.create(dirname(upstream_rds), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(value = 1), upstream_rds)

  base
}

.chapter_by_key <- function(manifest, key) {
  idx <- which(vapply(manifest$chapters, function(ch) identical(ch$chapter, key), logical(1)))
  if (length(idx) != 1L) return(NULL)
  manifest$chapters[[idx]]
}


# ===========================================================================
# Property 13: Repro manifest contains required fields for all chapters
# Feature: portfolio-packaging, Property 13: required manifest fields
# **Validates: Requirements 8.1**
# ===========================================================================

test_that("Property 13: executed chapters include checksums, seed metadata, and timestamp", {
  fixture_root <- .create_manifest_fixture()
  on.exit(unlink(fixture_root, recursive = TRUE), add = TRUE)

  cfg_path <- file.path(fixture_root, "reproduced", "config", "thesis.yml")
  bundle <- MANIFEST_ENV$load_configuration(cfg_path)
  manifest <- MANIFEST_ENV$build_manifest(bundle)

  expect_true(is.list(manifest))
  expect_true(nzchar(manifest$generated_at))
  expect_equal(manifest$data_mode, "proxy")
  expect_equal(manifest$pipeline_version, "0.0.1")
  expect_equal(length(manifest$chapters), 4L)

  required_chapters <- c(
    "chapter4_data_collection",
    "chapter5_descriptive_norms",
    "chapter6_injunctive_norms",
    "chapter7_saom"
  )

  for (key in required_chapters) {
    ch <- .chapter_by_key(manifest, key)
    expect_false(is.null(ch), info = paste("Missing chapter manifest:", key))
    expect_equal(ch$status, "executed", info = paste("Chapter should be executed:", key))
    expect_true(length(ch$input_checksums) >= 1L, info = paste("Missing input checksums:", key))
    expect_true(length(ch$output_checksums) >= 1L, info = paste("Missing output checksums:", key))
    expect_true(is.character(ch$execution_timestamp) && nzchar(ch$execution_timestamp),
                info = paste("Missing execution timestamp:", key))
  }

  expect_equal(.chapter_by_key(manifest, "chapter5_descriptive_norms")$rng_seed, 20250921)
  expect_equal(.chapter_by_key(manifest, "chapter7_saom")$rng_seed, 2022)
})

test_that("Property 13: write_manifest emits parseable JSON file", {
  fixture_root <- .create_manifest_fixture()
  on.exit(unlink(fixture_root, recursive = TRUE), add = TRUE)

  cfg_path <- file.path(fixture_root, "reproduced", "config", "thesis.yml")
  bundle <- MANIFEST_ENV$load_configuration(cfg_path)
  manifest <- MANIFEST_ENV$build_manifest(bundle)
  out_path <- MANIFEST_ENV$write_manifest(bundle, manifest)

  expect_true(file.exists(out_path))
  parsed <- jsonlite::read_json(out_path, simplifyVector = FALSE)
  expect_true("chapters" %in% names(parsed))
  expect_equal(length(parsed$chapters), 4L)
})


# ===========================================================================
# Property 16: Deterministic outputs preserve checksums across runs
# Feature: portfolio-packaging, Property 16: deterministic checksums
# **Validates: Requirements 8.2**
# ===========================================================================

test_that("Property 16: repeated manifest builds keep Chapter 4-6 output checksums identical", {
  fixture_root <- .create_manifest_fixture()
  on.exit(unlink(fixture_root, recursive = TRUE), add = TRUE)

  cfg_path <- file.path(fixture_root, "reproduced", "config", "thesis.yml")
  bundle <- MANIFEST_ENV$load_configuration(cfg_path)

  m1 <- MANIFEST_ENV$build_manifest(bundle)
  m2 <- MANIFEST_ENV$build_manifest(bundle)

  compare_keys <- c("chapter4_data_collection", "chapter5_descriptive_norms", "chapter6_injunctive_norms")
  for (key in compare_keys) {
    c1 <- .chapter_by_key(m1, key)$output_checksums
    c2 <- .chapter_by_key(m2, key)$output_checksums
    expect_equal(c1, c2, info = paste("Output checksums changed for", key))
  }
})
