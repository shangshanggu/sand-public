#!/usr/bin/env Rscript

format_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

find_repo_root <- function() {
  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    candidate <- file.path(current, "reproduced", "config", "thesis.yml")
    if (file.exists(candidate)) {
      return(normalizePath(current, winslash = "/", mustWork = TRUE))
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    current <- parent
  }
  stop("Unable to locate repository root containing reproduced/config/thesis.yml")
}

source_config_loader <- function(repo_root) {
  loader_path <- file.path(repo_root, "reproduced", "scripts", "utils", "config_loader.R")
  if (!file.exists(loader_path)) {
    stop("config_loader.R is missing; cannot continue.")
  }
  source(loader_path)
}

parse_arguments <- function(args) {
  config <- NULL
  formats <- character()
  quiet <- FALSE
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--config")) {
      if (i == length(args)) {
        stop("--config flag requires a path argument")
      }
      config <- args[[i + 1L]]
      i <- i + 2L
    } else if (identical(arg, "--format")) {
      if (i == length(args)) {
        stop("--format flag requires a format value")
      }
      formats <- c(formats, args[[i + 1L]])
      i <- i + 2L
    } else if (identical(arg, "--quiet")) {
      quiet <- TRUE
      i <- i + 1L
    } else if (identical(arg, "--help") || identical(arg, "-h")) {
      cat(
        "Usage: render_thesis.R [--config <path>] [--format <fmt>] [--quiet]\\n",
        "\\n",
        "Builds thesis-ready artefacts by consuming packaged tables/figures",
        " and emitting LaTeX/PDF summaries ready for Quarto integration.\\n",
        sep = ""
      )
      quit(save = "no", status = 0L)
    } else {
      stop(sprintf("Unknown argument: %s", arg))
    }
  }
  list(config = config, formats = formats, quiet = quiet)
}

ensure_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) {
    return(invisible(TRUE))
  }
  stop(
    sprintf(
      "Missing required R packages: %s. Install them via `make env-renv`.",
      paste(missing, collapse = ", ")
    )
  )
}

relative_path <- function(path, base) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  base <- normalizePath(base, winslash = "/", mustWork = TRUE)
  sub(paste0("^", base, "/"), "", path)
}

latex_escape <- function(text) {
  text <- gsub("\\\\", "\\\\textbackslash{}", text)
  text <- gsub("([{}_#%&$])", "\\\\\\1", text, perl = TRUE)
  text <- gsub("~", "\\\\textasciitilde{}", text, fixed = TRUE)
  gsub("\\^", "\\\\textasciicircum{}", text)
}

load_manifests <- function(tables_manifest_path, figures_manifest_path) {
  ensure_packages("jsonlite")
  tables <- jsonlite::read_json(tables_manifest_path, simplifyVector = FALSE)
  figures <- jsonlite::read_json(figures_manifest_path, simplifyVector = FALSE)
  list(tables = tables, figures = figures)
}

summarise_entries <- function(manifest) {
  if (!length(manifest$assets)) {
    return(list())
  }
  lapply(manifest$assets, function(entry) {
    list(
      chapter = entry$chapter,
      asset_type = entry$asset_type,
      destination = entry$destination$relative,
      source = entry$source$relative,
      size_bytes = entry$size_bytes
    )
  })
}

format_size <- function(bytes) {
  units <- c("B", "KB", "MB", "GB")
  if (is.null(bytes) || is.na(bytes)) {
    return("0 B")
  }
  magnitude <- 0L
  value <- as.numeric(bytes)
  while (value >= 1024 && magnitude < length(units) - 1L) {
    value <- value / 1024
    magnitude <- magnitude + 1L
  }
  sprintf("%.1f %s", value, units[[magnitude + 1L]])
}

generate_latex_summary <- function(manifests, dest_dir, repo_root, timestamp, quiet) {
  dest_dir <- normalizePath(dest_dir, winslash = "/", mustWork = FALSE)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path(dest_dir, sprintf("thesis_assets_%s.tex", format(Sys.time(), "%Y%m%dT%H%M%S")))
  table_entries <- summarise_entries(manifests$tables)
  figure_entries <- summarise_entries(manifests$figures)
  lines <- c(
    "\\documentclass{article}",
    "\\usepackage[margin=1in]{geometry}",
    "\\begin{document}",
    sprintf("\\section*{Thesis Asset Summary (%s)}", latex_escape(timestamp)),
    sprintf("Total tables: %d (%s)", length(table_entries), format_size(manifests$tables$totals$size_bytes)),
    sprintf("Total figures: %d (%s)", length(figure_entries), format_size(manifests$figures$totals$size_bytes))
  )
  lines <- c(lines, "\\subsection*{Tables}")
  if (!length(table_entries)) {
    lines <- c(lines, "No tables were packaged in this run.")
  } else {
    lines <- c(lines, "\\begin{enumerate}")
    for (entry in table_entries) {
      lines <- c(
        lines,
        sprintf(
          "  \\item Chapter %s: \\texttt{%s} (source: \\texttt{%s}, size: %s)",
          latex_escape(entry$chapter),
          latex_escape(entry$destination),
          latex_escape(entry$source),
          format_size(entry$size_bytes)
        )
      )
    }
    lines <- c(lines, "\\end{enumerate}")
  }
  lines <- c(lines, "\\subsection*{Figures}")
  if (!length(figure_entries)) {
    lines <- c(lines, "No figures were packaged in this run.")
  } else {
    lines <- c(lines, "\\begin{enumerate}")
    for (entry in figure_entries) {
      lines <- c(
        lines,
        sprintf(
          "  \\item Chapter %s: \\texttt{%s} (source: \\texttt{%s}, size: %s)",
          latex_escape(entry$chapter),
          latex_escape(entry$destination),
          latex_escape(entry$source),
          format_size(entry$size_bytes)
        )
      )
    }
    lines <- c(lines, "\\end{enumerate}")
  }
  lines <- c(lines, "\\subsection*{Manifests}")
  lines <- c(
    lines,
    sprintf("Tables manifest: \\texttt{%s}", latex_escape(relative_path(manifests$tables$config$config_path, repo_root))),
    sprintf("Figures manifest: \\texttt{%s}", latex_escape(relative_path(manifests$figures$config$config_path, repo_root)))
  )
  lines <- c(lines, "\\end{document}")
  writeLines(lines, output_path)
  if (!quiet) {
    message("[render-thesis] Wrote LaTeX summary to ", relative_path(output_path, repo_root))
  }
  output_path
}

generate_pdf_summary <- function(manifests, dest_dir, repo_root, timestamp, quiet) {
  dest_dir <- normalizePath(dest_dir, winslash = "/", mustWork = FALSE)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path(dest_dir, sprintf("thesis_assets_%s.pdf", format(Sys.time(), "%Y%m%dT%H%M%S")))
  table_entries <- summarise_entries(manifests$tables)
  figure_entries <- summarise_entries(manifests$figures)
  grDevices::pdf(output_path, width = 8.5, height = 11)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::grid.text(
    sprintf("Thesis Asset Summary (%s)", timestamp),
    x = 0.5,
    y = 0.95,
    gp = grid::gpar(fontsize = 16, fontface = "bold")
  )
  y <- 0.88
  summary_lines <- c(
    sprintf("Total tables: %d (%s)", length(table_entries), format_size(manifests$tables$totals$size_bytes)),
    sprintf("Total figures: %d (%s)", length(figure_entries), format_size(manifests$figures$totals$size_bytes)),
    sprintf("Tables manifest: %s", relative_path(manifests$tables$config$config_path, repo_root)),
    sprintf("Figures manifest: %s", relative_path(manifests$figures$config$config_path, repo_root))
  )
  for (line in summary_lines) {
    grid::grid.text(line, x = 0.05, y = y, just = c("left", "top"), gp = grid::gpar(fontsize = 10))
    y <- y - 0.04
  }
  if (length(table_entries)) {
    grid::grid.text("Tables:", x = 0.05, y = y, just = c("left", "top"), gp = grid::gpar(fontsize = 12, fontface = "bold"))
    y <- y - 0.035
    for (entry in head(table_entries, 20L)) {
      line <- sprintf("%s -> %s", entry$chapter, entry$destination)
      grid::grid.text(line, x = 0.07, y = y, just = c("left", "top"), gp = grid::gpar(fontsize = 9))
      y <- y - 0.03
      if (y < 0.1) break
    }
  }
  if (length(figure_entries) && y > 0.15) {
    grid::grid.text("Figures:", x = 0.05, y = y, just = c("left", "top"), gp = grid::gpar(fontsize = 12, fontface = "bold"))
    y <- y - 0.035
    for (entry in head(figure_entries, 20L)) {
      line <- sprintf("%s -> %s", entry$chapter, entry$destination)
      grid::grid.text(line, x = 0.07, y = y, just = c("left", "top"), gp = grid::gpar(fontsize = 9))
      y <- y - 0.03
      if (y < 0.1) break
    }
  }
  if (!quiet) {
    message("[render-thesis] Wrote PDF summary to ", relative_path(output_path, repo_root))
  }
  output_path
}

attempt_quarto_render <- function(quarto_cli, source_file, format, output_dir, project_dir, quiet) {
  args <- c("render", source_file, "--to", format, "--output-dir", output_dir)
  status <- system2(quarto_cli, args = args, stdout = if (quiet) FALSE else "", stderr = if (quiet) FALSE else "")
  status == 0L
}

main <- function() {
  args <- parse_arguments(commandArgs(trailingOnly = TRUE))
  repo_root <- find_repo_root()
  source_config_loader(repo_root)

  config_path <- args$config
  if (is.null(config_path)) {
    config_path <- file.path(repo_root, "reproduced", "config", "thesis.yml")
  }
  config_bundle <- load_configuration(config_path)
  build_cfg <- get_config_value(config_bundle, "thesis", "build", required = TRUE)
  pack_cfg <- get_config_value(config_bundle, "thesis", "packaging", required = TRUE)

  tables_manifest_path <- resolve_repo_path(config_bundle, pack_cfg$tables_manifest, must_exist = TRUE)
  figures_manifest_path <- resolve_repo_path(config_bundle, pack_cfg$figures_manifest, must_exist = TRUE)
  manifests <- load_manifests(tables_manifest_path, figures_manifest_path)

  formats <- args$formats
  if (!length(formats)) {
    formats <- build_cfg$default_formats
  }
  formats <- unique(tolower(formats))

  paths_section <- get_config_value(config_bundle, "project", "paths", required = TRUE)
  tex_output_dir <- resolve_repo_path(config_bundle, paths_section$thesis_tex_dir, must_exist = FALSE)
  pdf_output_dir <- resolve_repo_path(config_bundle, paths_section$thesis_pdf_dir, must_exist = FALSE)

  source_file <- resolve_repo_path(config_bundle, build_cfg$source_file, must_exist = FALSE)
  project_dir <- resolve_repo_path(config_bundle, build_cfg$project_dir, must_exist = FALSE)
  quarto_cli <- Sys.which("quarto")
  quarto_available <- nzchar(quarto_cli) && file.exists(source_file)

  timestamp <- format_timestamp()
  outputs <- list()

  if ("latex" %in% formats || "tex" %in% formats) {
    if (quarto_available) {
      success <- attempt_quarto_render(quarto_cli, source_file, "latex", tex_output_dir, project_dir, args$quiet)
      if (!success && !args$quiet) {
        message("[render-thesis] Quarto render failed; generating summary LaTeX instead.")
      }
      if (!success) {
        outputs$latex <- generate_latex_summary(manifests, tex_output_dir, repo_root, timestamp, args$quiet)
      } else {
        outputs$latex <- file.path(tex_output_dir, paste0(tools::file_path_sans_ext(basename(source_file)), ".tex"))
      }
    } else {
      if (!args$quiet) {
        message("[render-thesis] Quarto CLI not available or source missing; generating summary LaTeX.")
      }
      outputs$latex <- generate_latex_summary(manifests, tex_output_dir, repo_root, timestamp, args$quiet)
    }
  }

  if ("pdf" %in% formats) {
    if (quarto_available) {
      success <- attempt_quarto_render(quarto_cli, source_file, "pdf", pdf_output_dir, project_dir, args$quiet)
      if (!success && !args$quiet) {
        message("[render-thesis] Quarto PDF render failed; generating summary PDF instead.")
      }
      if (!success) {
        outputs$pdf <- generate_pdf_summary(manifests, pdf_output_dir, repo_root, timestamp, args$quiet)
      } else {
        outputs$pdf <- file.path(pdf_output_dir, paste0(tools::file_path_sans_ext(basename(source_file)), ".pdf"))
      }
    } else {
      if (!args$quiet) {
        message("[render-thesis] Quarto CLI not available or source missing; generating summary PDF.")
      }
      outputs$pdf <- generate_pdf_summary(manifests, pdf_output_dir, repo_root, timestamp, args$quiet)
    }
  }

  if (!args$quiet) {
    message("[render-thesis] Generated formats: ", paste(names(outputs), collapse = ", "))
  }
}

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(e) {
      message("[render-thesis] Error: ", e$message)
      quit(status = 1L)
    }
  )
}
