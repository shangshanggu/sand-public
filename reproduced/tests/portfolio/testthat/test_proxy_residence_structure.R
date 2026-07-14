test_that("proxy data exercise blockmate and flatmate covariates", {
  candidates <- c(
    file.path(PROXY_DATA_DIR, "list_by_wave.RData"),
    file.path(getwd(), "data", "proxy", "list_by_wave.RData"),
    file.path(getwd(), "..", "..", "data", "proxy", "list_by_wave.RData"),
    file.path(getwd(), "..", "..", "..", "data", "proxy", "list_by_wave.RData")
  )
  data_path <- candidates[file.exists(candidates)][1]
  expect_true(file.exists(data_path), info = "Run `make proxy-data` before the test suite.")

  env <- new.env(parent = emptyenv())
  load(data_path, envir = env)
  expect_true(exists("list_by_wave", envir = env, inherits = FALSE))
  list_by_wave <- get("list_by_wave", envir = env, inherits = FALSE)

  required <- c("redcap_survey_identifier", "number_block", "number_flat")
  for (wave in list_by_wave) {
    expect_true(
      all(required %in% names(wave)),
      info = "Every proxy wave must carry stable synthetic block and flat fields."
    )
  }

  base <- list_by_wave[[1]][!duplicated(list_by_wave[[1]]$redcap_survey_identifier), required]
  expect_false(anyNA(base))
  expect_gt(length(unique(base$number_block)), 1L)
  expect_gt(length(unique(paste(base$number_block, base$number_flat, sep = "::"))), 1L)

  same_block <- outer(base$number_block, base$number_block, `==`)
  flat_key <- paste(base$number_block, base$number_flat, sep = "::")
  same_flat <- outer(flat_key, flat_key, `==`)
  diag(same_block) <- FALSE
  diag(same_flat) <- FALSE

  expect_gt(sum(same_flat), 0L)
  expect_gt(sum(same_block), sum(same_flat))

  reference <- setNames(
    paste(base$number_block, base$number_flat, sep = "::"),
    base$redcap_survey_identifier
  )
  for (wave in list_by_wave[-1]) {
    wave_unique <- wave[!duplicated(wave$redcap_survey_identifier), required]
    expect_equal(
      paste(wave_unique$number_block, wave_unique$number_flat, sep = "::"),
      unname(reference[as.character(wave_unique$redcap_survey_identifier)]),
      info = "Synthetic residence assignments must remain stable across waves."
    )
  }
})
