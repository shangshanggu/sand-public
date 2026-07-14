#!/usr/bin/env Rscript

suppressPackageStartupMessages({
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
  stop("Unable to determine run_all.R location.")
}

relative_repo_path <- function(path, repo_root) {
  repo_norm <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (identical(path_norm, repo_norm)) {
    return(".")
  }
  if (startsWith(path_norm, paste0(repo_norm, "/"))) {
    return(substring(path_norm, nchar(repo_norm) + 2))
  }
  path_norm
}

append_run_history <- function(log_path, record) {
  append_run_log(log_path, record)
}

run_command <- function(label, command, args = character()) {
  message(sprintf("[run_all] %s", label))
  status <- system2(command, args = args)
  if (!identical(status, 0L)) {
    stop(sprintf("Step '%s' failed with status %s", label, status))
  }
}

main <- function() {
  script_path <- get_script_path()
  repro_root <- dirname(script_path)
  repo_root <- normalizePath(file.path(repro_root, ".."), winslash = "/", mustWork = TRUE)
  setwd(repo_root)

  source(file.path(repro_root, "scripts", "utils", "config_loader.R"))
  source(file.path(repro_root, "R", "common.R"))
  bundle <- load_configuration(file.path(repro_root, "config", "thesis.yml"))

  python_bin <- Sys.which("python3")
  if (!nzchar(python_bin)) {
    python_bin <- Sys.which("python")
  }
  if (!nzchar(python_bin)) {
    stop("python3 (or python) command not found in PATH.")
  }
  r_bin <- Sys.which("Rscript")
  if (!nzchar(r_bin)) {
    stop("Rscript command not found in PATH.")
  }

  logs_rel <- get_config_value(bundle, "project", "paths", "logs_dir", default = "logs")
  logs_dir <- resolve_repo_path(bundle, logs_rel, must_exist = FALSE)
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(logs_dir, "run_all_history.json")

  chapters_run <- c("chapter4", "chapter5", "chapter6", "chapter7")
  start_time <- Sys.time()
  steps <- list()
  status <- "success"
  error_message <- NULL

  record <- list(
    started_at = format_timestamp(start_time),
    config = relative_repo_path(bundle$config_path, repo_root),
    project = list(
      name = get_config_value(bundle, "project", "name", default = NA_character_),
      version = get_config_value(bundle, "project", "version", default = NA_character_),
      timezone = get_config_value(bundle, "project", "timezone", default = NA_character_)
    ),
    chapters = chapters_run,
    steps = list()
  )

  on.exit({
    end_time <- Sys.time()
    record$finished_at <- format_timestamp(end_time)
    record$status <- status
    record$duration_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))
    record$steps <- steps
    if (!is.null(error_message)) {
      record$error <- list(message = error_message)
    }
    append_run_history(log_path, record)
  }, add = TRUE)

  tryCatch({
    validate_script <- file.path(repro_root, "scripts", "00_setup", "validate_config.py")
    run_command(
      "validate-config",
      python_bin,
      c(validate_script, "--config", bundle$config_path)
    )
    steps <- c(steps, list(list(
      name = "validate-config",
      command = relative_repo_path(validate_script, repo_root),
      args = list("--config", relative_repo_path(bundle$config_path, repo_root))
    )))

    ensure_enabled <- function(key) {
      enabled <- get_config_value(bundle, "chapters", key, "enabled", default = FALSE)
      if (!isTRUE(enabled)) {
        stop(sprintf("Chapter '%s' is disabled in reproduced/config/thesis.yml.", key))
      }
    }

    ensure_enabled("chapter4_data_collection")
    ch4_dir <- resolve_repo_path(bundle, get_config_value(bundle, "chapters", "chapter4_data_collection", "scripts_dir", required = TRUE), must_exist = TRUE)
    ch4_scripts <- sort(list.files(ch4_dir, pattern = "\\.R$", full.names = TRUE))
    for (script in ch4_scripts) {
      run_command(sprintf("chapter4: %s", basename(script)), r_bin, c(script))
      steps <- c(steps, list(list(
        name = "chapter4",
        command = relative_repo_path(script, repo_root)
      )))
    }

    ensure_enabled("chapter5_descriptive_norms")
    ch5_dir <- resolve_repo_path(bundle, get_config_value(bundle, "chapters", "chapter5_descriptive_norms", "scripts_dir", required = TRUE), must_exist = TRUE)
    ch5_scripts <- sort(list.files(ch5_dir, pattern = "\\.R$", full.names = TRUE))
    for (script in ch5_scripts) {
      run_command(sprintf("chapter5: %s", basename(script)), r_bin, c(script))
      steps <- c(steps, list(list(
        name = "chapter5",
        command = relative_repo_path(script, repo_root)
      )))
    }

    ensure_enabled("chapter6_injunctive_norms")
    ch6_dir <- resolve_repo_path(bundle, get_config_value(bundle, "chapters", "chapter6_injunctive_norms", "scripts_dir", required = TRUE), must_exist = TRUE)
    ch6_scripts <- sort(list.files(ch6_dir, pattern = "\\.R$", full.names = TRUE))
    for (script in ch6_scripts) {
      run_command(sprintf("chapter6: %s", basename(script)), r_bin, c(script))
      steps <- c(steps, list(list(
        name = "chapter6",
        command = relative_repo_path(script, repo_root)
      )))
    }

    ensure_enabled("chapter7_saom")
    rsiena_script <- resolve_repo_path(bundle, get_config_value(bundle, "rsiena", "installation", "script_path", required = TRUE), must_exist = TRUE)
    run_command("rsiena-install", r_bin, c(rsiena_script))
    steps <- c(steps, list(list(
      name = "rsiena",
      command = relative_repo_path(rsiena_script, repo_root)
    )))

    ch7_dir <- resolve_repo_path(bundle, get_config_value(bundle, "chapters", "chapter7_saom", "scripts_dir", required = TRUE), must_exist = TRUE)
    ch7_scripts <- sort(list.files(ch7_dir, pattern = "\\.R$", full.names = TRUE))
    default_model <- get_config_value(bundle, "chapters", "chapter7_saom", "default_model", default = "base")
    for (script in ch7_scripts) {
      if (grepl("02_run_saom_model\\.R$", script)) {
        run_command(sprintf("chapter7: %s", basename(script)), r_bin, c(script, "--model", default_model))
        steps <- c(steps, list(list(
          name = "chapter7",
          command = relative_repo_path(script, repo_root),
          args = list("--model", default_model)
        )))
      } else {
        run_command(sprintf("chapter7: %s", basename(script)), r_bin, c(script))
        steps <- c(steps, list(list(
          name = "chapter7",
          command = relative_repo_path(script, repo_root)
        )))
      }
    }
  }, error = function(e) {
    status <<- "failed"
    error_message <<- conditionMessage(e)
  })

  if (!is.null(error_message)) {
    stop(error_message)
  }

  message("[run_all] Pipeline completed successfully.")
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("Error executing run_all.R: ", e$message)
      quit(status = 1)
    }
  )
}
