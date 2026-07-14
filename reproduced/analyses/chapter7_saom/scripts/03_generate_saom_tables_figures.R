#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The 'yaml' package is required. Install it via install.packages('yaml').", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The 'jsonlite' package is required. Install it via install.packages('jsonlite').", call. = FALSE)
  }
})

library(jsonlite)
library(yaml)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  if (is.character(x) && length(x) == 1 && !nzchar(x)) {
    return(y)
  }
  x
}

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matches <- grep(file_arg, args, fixed = TRUE)
  if (length(matches) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", args[matches[1]]), winslash = "/", mustWork = TRUE)))
  }
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

parse_args <- function(default_model) {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- list(model = default_model, config = "config/thesis.yml")
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--model") {
      if (i == length(args)) stop("--model requires an identifier.")
      i <- i + 1
      opts$model <- args[[i]]
    } else if (arg == "--config") {
      if (i == length(args)) stop("--config requires a path.")
      i <- i + 1
      opts$config <- args[[i]]
    } else if (arg %in% c("--help", "-h")) {
      cat("Usage: Rscript 03_generate_saom_tables_figures.R [--model <id>] [--config <path>]\n")
      quit(status = 0)
    } else {
      stop(sprintf("Unrecognised argument '%s'", arg))
    }
    i <- i + 1
  }
  opts
}

normalise_effect_names <- function(theta_matrix, summary_obj) {
  if (is.null(theta_matrix)) {
    return(list(matrix = NULL, effect_names = character(0), labels = character(0)))
  }
  effect_names <- rownames(theta_matrix)
  labels <- effect_names
  if (!is.null(summary_obj) && !is.null(summary_obj$effects)) {
    effect_labels <- tryCatch(summary_obj$effects$effectName, error = function(...) NULL)
    if (!is.null(effect_labels) && length(effect_labels) >= nrow(theta_matrix)) {
      labels <- effect_labels[seq_len(nrow(theta_matrix))]
    }
  }
  if (is.null(effect_names) || any(!nzchar(effect_names))) {
    effect_names <- paste0("effect_", seq_len(nrow(theta_matrix)))
  }
  list(matrix = theta_matrix, effect_names = effect_names, labels = labels)
}

extract_effect_metadata <- function(summary_obj, n_effects) {
  effects_df <- tryCatch(as.data.frame(summary_obj$effects), error = function(...) NULL)
  if (is.null(effects_df) || !nrow(effects_df)) {
    return(list(
      label = rep(NA_character_, n_effects),
      short_name = rep(NA_character_, n_effects),
      interaction1 = rep(NA_character_, n_effects),
      interaction2 = rep(NA_character_, n_effects),
      type = rep(NA_character_, n_effects),
      name = rep(NA_character_, n_effects)
    ))
  }

  take <- seq_len(min(n_effects, nrow(effects_df)))
  pad_chr <- function(x) {
    x <- as.character(x)
    if (length(x) < n_effects) {
      x <- c(x, rep(NA_character_, n_effects - length(x)))
    }
    x[seq_len(n_effects)]
  }

  list(
    label = pad_chr(effects_df$effectName[take]),
    short_name = pad_chr(effects_df$shortName[take]),
    interaction1 = pad_chr(effects_df$interaction1[take]),
    interaction2 = pad_chr(effects_df$interaction2[take]),
    type = pad_chr(effects_df$type[take]),
    name = pad_chr(effects_df$name[take])
  )
}

write_coefficients_table <- function(df, path_csv, path_json) {
  dir.create(dirname(path_csv), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(path_json), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, path_csv, row.names = FALSE)
  jsonlite::write_json(list(coefficients = df), path_json, pretty = TRUE, auto_unbox = TRUE, na = "null")
}

render_coefficient_plot <- function(df, path_png, title) {
  dir.create(dirname(path_png), recursive = TRUE, showWarnings = FALSE)
  if (!nrow(df)) {
    warning("No coefficients available for plotting; skipping figure generation.")
    return(invisible(FALSE))
  }
  ordered <- df[order(df$estimate, decreasing = TRUE), , drop = FALSE]
  labels <- ordered$label %||% ordered$effect
  png(filename = path_png, width = 1600, height = 900, res = 150)
  on.exit(dev.off(), add = TRUE)
  par(mar = c(5, max(8, length(labels) * 0.6), 4, 2))
  plot(
    ordered$estimate,
    seq_along(ordered$estimate),
    xlab = "Estimate",
    ylab = "",
    main = title,
    yaxt = "n",
    pch = 19,
    col = ifelse(isTRUE(ordered$significant), "#1b9e77", "#757575")
  )
  axis(2, at = seq_along(labels), labels = labels, las = 1, cex.axis = 0.8)
  if (!all(is.na(ordered$std_error))) {
    se <- ordered$std_error
    x0 <- ordered$estimate - 1.96 * se
    x1 <- ordered$estimate + 1.96 * se
    segments(x0, seq_along(se), x1, seq_along(se), col = "#555555")
  }
  abline(v = 0, lty = 3, col = "#999999")
  invisible(TRUE)
}

relative_or_absolute <- function(path, repo_root) {
  if (!nzchar(path)) {
    return(NULL)
  }
  if (exists("relative_repo_path", mode = "function")) {
    return(relative_repo_path(path, repo_root))
  }
  path
}

main <- function() {
  script_dir <- get_script_dir()
  repo_root <- normalizePath(file.path(script_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
  setwd(repo_root)

  source(file.path(repo_root, "scripts", "utils", "config_loader.R"))
  source(file.path(repo_root, "R", "common.R"))

  config_bundle <- load_configuration(file.path(repo_root, "config", "thesis.yml"))
  chapter_cfg <- config_bundle$config$chapters$chapter7_saom
  if (is.null(chapter_cfg)) {
    stop("chapter7_saom configuration block missing from thesis.yml")
  }
  args <- parse_args(chapter_cfg$default_model %||% names(chapter_cfg$target_coefficients)[1] %||% "base")

  if (!identical(normalizePath(args$config, winslash = "/", mustWork = FALSE), config_bundle$config_path)) {
    config_bundle <- load_configuration(args$config)
    chapter_cfg <- config_bundle$config$chapters$chapter7_saom
    if (is.null(chapter_cfg)) {
      stop("chapter7_saom configuration block missing from provided configuration")
    }
  }

  rsiena_install_cfg <- config_bundle$config$rsiena$installation %||% list()
  rsiena_library_rel <- rsiena_install_cfg$library_dir %||% NULL
  if (!is.null(rsiena_library_rel) && nzchar(rsiena_library_rel)) {
    rsiena_library_dir <- resolve_repo_path(config_bundle, rsiena_library_rel, must_exist = FALSE)
    if (!is.null(rsiena_library_dir) && dir.exists(rsiena_library_dir)) {
      .libPaths(c(rsiena_library_dir, .libPaths()))
    }
  }
  requireNamespace("RSiena", quietly = TRUE)

  model_id <- args$model
  output_paths <- resolve_chapter_output_paths(config_bundle, "chapter7_saom", create = TRUE)
  logs_dir <- file.path(output_paths$base, "logs")
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

  run_log_path <- file.path(logs_dir, sprintf("saom_run_%s.json", model_id))
  if (!file.exists(run_log_path)) {
    stop(sprintf("SAOM run log not found at %s. Run 02_run_saom_model.R first.", run_log_path))
  }

  run_log <- jsonlite::fromJSON(run_log_path, simplifyVector = FALSE)
  fit_path <- run_log$fit_path %||% file.path(output_paths$base, "cache", sprintf("%s_fit.RData", model_id))
  placeholder_fit <- isTRUE(run_log$placeholder)

  coeff_df <- data.frame()
  std_error <- numeric(0)
  p_value <- numeric(0)
  labels <- character(0)

  if (!is.null(fit_path) && file.exists(fit_path)) {
    fit_env <- new.env(parent = emptyenv())
    load(fit_path, envir = fit_env)
    result_obj <- fit_env$result
    summary_obj <- tryCatch(suppressWarnings(summary(result_obj)), error = function(err) NULL)

    # Primary extraction: pull estimates/SEs directly from the sienaFit object.
    # RSiena stores result$theta (estimates) and result$se (SEs) as vectors.
    direct_theta <- tryCatch(as.numeric(result_obj$theta), error = function(...) NULL)
    direct_se    <- tryCatch(as.numeric(result_obj$se),    error = function(...) NULL)

    if (!is.null(direct_theta) && length(direct_theta) > 0) {
      estimates <- direct_theta
      se <- if (!is.null(direct_se) && length(direct_se) == length(estimates)) direct_se else rep(NA_real_, length(estimates))
      z_values <- ifelse(is.na(se) | se == 0, NA_real_, estimates / se)
      p_values <- ifelse(is.na(z_values), NA_real_, 2 * pnorm(-abs(z_values)))

      meta <- extract_effect_metadata(
        if (!is.null(summary_obj)) summary_obj else result_obj,
        length(estimates)
      )
      # Prefer effectName from the fit object itself
      eff_names <- tryCatch(result_obj$effects$effectName, error = function(...) NULL)
      if (is.null(eff_names) || length(eff_names) != length(estimates)) {
        eff_names <- meta$label
      }
      if (is.null(eff_names) || length(eff_names) != length(estimates)) {
        eff_names <- paste0("effect_", seq_len(length(estimates)))
      }

      coeff_df <- data.frame(
        effect = eff_names,
        estimate = estimates,
        std_error = se,
        z_value = z_values,
        p_value = p_values,
        label = meta$label,
        short_name = meta$short_name,
        interaction1 = meta$interaction1,
        interaction2 = meta$interaction2,
        type = meta$type,
        dependent = meta$name,
        stringsAsFactors = FALSE
      )
    } else {
      # Fallback: try summary_obj$theta (works for placeholder fits)
      theta_obj <- if (!is.null(summary_obj)) summary_obj$theta else NULL
      if (!is.null(theta_obj) && is.numeric(theta_obj) && length(theta_obj) > 0) {
        estimates <- as.numeric(theta_obj)
        se <- tryCatch(as.numeric(summary_obj$se), error = function(...) rep(NA_real_, length(estimates)))
        if (length(se) != length(estimates)) {
          se <- rep(NA_real_, length(estimates))
        }
        z_values <- ifelse(is.na(se) | se == 0, NA_real_, estimates / se)
        p_values <- ifelse(is.na(z_values), NA_real_, 2 * pnorm(-abs(z_values)))

        meta <- extract_effect_metadata(summary_obj, length(estimates))
        coeff_df <- data.frame(
          effect = paste0("effect_", seq_len(length(estimates))),
          estimate = estimates,
          std_error = se,
          z_value = z_values,
          p_value = p_values,
          label = meta$label,
          short_name = meta$short_name,
          interaction1 = meta$interaction1,
          interaction2 = meta$interaction2,
          type = meta$type,
          dependent = meta$name,
          stringsAsFactors = FALSE
        )
      } else {
        theta_matrix <- tryCatch({
          if (!is.null(theta_obj)) {
            as.matrix(theta_obj)
          } else {
            NULL
          }
        }, error = function(err) NULL)

        normalised <- normalise_effect_names(theta_matrix, summary_obj)
        theta_matrix <- normalised$matrix
        effect_names <- normalised$effect_names
        labels <- normalised$labels

        if (!is.null(theta_matrix) && length(effect_names) > 0) {
          coeff_df <- data.frame(
            effect = effect_names,
            estimate = as.numeric(theta_matrix[, 1]),
            stringsAsFactors = FALSE
          )
          if (ncol(theta_matrix) >= 2) {
            coeff_df$std_error <- as.numeric(theta_matrix[, 2])
          }
          if (ncol(theta_matrix) >= 4) {
            coeff_df$p_value <- as.numeric(theta_matrix[, 4])
          } else if (ncol(theta_matrix) >= 3) {
            coeff_df$p_value <- as.numeric(theta_matrix[, 3])
          }
        }
      }
    }
  }

  if (!nrow(coeff_df) && !is.null(run_log$theta)) {
    theta_entries <- run_log$theta
    coeff_df <- data.frame(
      effect = vapply(theta_entries, function(entry) entry$effect %||% NA_character_, character(1)),
      estimate = vapply(theta_entries, function(entry) entry$estimate %||% NA_real_, numeric(1)),
      stringsAsFactors = FALSE
    )
  }

  if (!nrow(coeff_df)) {
    stop("Unable to derive coefficient estimates from SAOM outputs.")
  }

  if (!"label" %in% names(coeff_df)) {
    if (!length(labels) || length(labels) < nrow(coeff_df)) {
      labels <- coeff_df$effect
    }
    coeff_df$label <- labels
  }
  if (!"std_error" %in% names(coeff_df)) {
    coeff_df$std_error <- NA_real_
  }
  if (!"p_value" %in% names(coeff_df)) {
    coeff_df$p_value <- NA_real_
  }
  coeff_df$significant <- ifelse(is.na(coeff_df$p_value), NA, coeff_df$p_value < 0.05)

  generated_at <- format(Sys.time(), tz = config_bundle$config$project$timezone %||% "UTC")
  tables_dir <- file.path(output_paths$base, "tables")
  figures_dir <- file.path(output_paths$base, "figures")
  manifests_dir <- file.path(output_paths$base, "manifests")
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(manifests_dir, recursive = TRUE, showWarnings = FALSE)

  csv_path <- file.path(tables_dir, sprintf("saom_coefficients_%s.csv", model_id))
  json_path <- file.path(tables_dir, sprintf("saom_coefficients_%s.json", model_id))
  figure_path <- file.path(figures_dir, sprintf("saom_coefficients_%s.png", model_id))

  write_coefficients_table(coeff_df, csv_path, json_path)
  render_coefficient_plot(coeff_df, figure_path, sprintf("SAOM Coefficients (%s)", model_id))

  manifest <- list(
    model = model_id,
    generated_at = generated_at,
    placeholder = if (placeholder_fit) TRUE else FALSE,
    sources = list(
      run_log = relative_or_absolute(run_log_path, config_bundle$repo_root),
      fit_path = if (!is.null(fit_path) && file.exists(fit_path)) relative_or_absolute(fit_path, config_bundle$repo_root) else NULL
    ),
    outputs = list(
      tables = list(
        coefficients_csv = relative_or_absolute(csv_path, config_bundle$repo_root),
        coefficients_json = relative_or_absolute(json_path, config_bundle$repo_root)
      ),
      figures = list(
        coefficients_plot = relative_or_absolute(figure_path, config_bundle$repo_root)
      )
    )
  )

  manifest_path <- file.path(manifests_dir, sprintf("saom_outputs_%s_manifest.json", model_id))
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE, na = "null")

  message(sprintf(
    "SAOM tables and figures generated for model '%s'. Table: %s, Figure: %s",
    model_id,
    csv_path,
    figure_path
  ))
}

if (identical(environment(), globalenv())) {
  tryCatch(main(), error = function(err) {
    message("Error generating SAOM tables/figures: ", conditionMessage(err))
    quit(status = 1)
  })
}
