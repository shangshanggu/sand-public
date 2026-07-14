#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# check_privacy.R
#
# Scan generated Markdown/HTML files for participant identifiers and missing
# synthetic-data provenance. The guard fails when it finds numeric IDs in
# participant contexts or an interactive network file without an explicit
# proxy label and machine-readable data-mode marker.
#
# Usage (from reproduced/):
#   Rscript scripts/portfolio/check_privacy.R [target_dir]
# -----------------------------------------------------------------------------

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_idx <- grep(file_arg, args)
  if (length(file_idx) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", args[file_idx[1]]),
                                 winslash = "/", mustWork = TRUE)))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile,
                                 winslash = "/", mustWork = TRUE)))
  }
  getwd()
}

# Returns integer-like tokens in participant-ID contexts.
detect_suspicious_ids <- function(line) {
  patterns <- c(
    "(?i)\\bparticipant\\s*(?:id|identifier)?\\s*[:=#-]?\\s*([0-9]{4,})\\b",
    "(?i)\\bredcap_survey_identifier\\s*[:=#-]?\\s*\"?([0-9]{4,})\"?\\b",
    "(?i)\\bnomination\\s*[:=#-]?\\s*\"?([0-9]{4,})\"?\\b",
    "\"(?:id|from|to|nomination|redcap_survey_identifier)\"\\s*:\\s*\"?([0-9]{4,})\"?"
  )

  findings <- character(0)
  for (pat in patterns) {
    matches <- regmatches(line, gregexpr(pat, line, perl = TRUE))[[1]]
    if (length(matches) == 0 || (length(matches) == 1 && identical(matches, character(0)))) {
      next
    }
    ids <- sub(".*?([0-9]{4,}).*", "\\1", matches, perl = TRUE)
    ids <- ids[nzchar(ids)]
    if (length(ids) > 0) {
      findings <- c(findings, ids)
    }
  }

  unique(findings)
}

scan_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  violations <- list()
  for (i in seq_along(lines)) {
    ids <- detect_suspicious_ids(lines[[i]])
    if (length(ids) == 0) next
    violations[[length(violations) + 1]] <- list(
      file = path,
      line = i,
      ids = ids,
      text = lines[[i]]
    )
  }
  violations
}

scan_directory <- function(target_dir) {
  files <- list.files(
    target_dir,
    pattern = "\\.(md|html)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  violations <- list()
  for (path in files) {
    file_hits <- scan_file(path)
    if (length(file_hits) > 0) {
      violations <- c(violations, file_hits)
    }
    if (grepl("^network_.*\\.html$", basename(path), ignore.case = TRUE)) {
      html <- paste(readLines(path, warn = FALSE), collapse = "\n")
      has_visible_label <- grepl("SYNTHETIC PROXY DATA", html, fixed = TRUE)
      has_mode_marker <- grepl(
        "data-sand-data-mode=[\"']proxy[\"']",
        html,
        perl = TRUE
      )
      if (!has_visible_label || !has_mode_marker) {
        violations[[length(violations) + 1]] <- list(
          file = path,
          line = 1L,
          ids = character(0),
          kind = "missing synthetic proxy provenance",
          text = "Interactive network HTML must carry a visible label and data-mode marker."
        )
      }
    }
  }

  list(files = files, violations = violations)
}

format_violation <- function(v, root = getwd()) {
  rel <- tryCatch({
    normalizePath(v$file, winslash = "/", mustWork = FALSE)
  }, error = function(...) v$file)
  root_abs <- normalizePath(root, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root_abs, "/")
  if (startsWith(rel, prefix)) {
    rel <- substring(rel, nchar(prefix) + 1L)
  }
  if (!is.null(v$kind)) {
    return(sprintf("%s:%d %s", rel, v$line, v$kind))
  }
  sprintf("%s:%d IDs=%s", rel, v$line, paste(v$ids, collapse = ","))
}

run_privacy_check <- function(target_dir = "outputs/portfolio") {
  if (!dir.exists(target_dir)) {
    stop(sprintf("Target directory does not exist: %s", target_dir))
  }

  result <- scan_directory(target_dir)
  if (length(result$files) == 0) {
    message(sprintf("[privacy] No .md/.html files found under %s; nothing to scan.", target_dir))
    return(invisible(TRUE))
  }

  if (length(result$violations) > 0) {
    message(sprintf("[privacy] FAILED: %d potential identifier leak(s) detected.",
                    length(result$violations)))
    for (v in result$violations) {
      message("[privacy] ", format_violation(v))
    }
    return(invisible(FALSE))
  }

  message(sprintf("[privacy] PASS: scanned %d file(s), no suspicious participant IDs found.",
                  length(result$files)))
  invisible(TRUE)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  target_dir <- if (length(args) >= 1) args[[1]] else "outputs/portfolio"
  ok <- run_privacy_check(target_dir)
  if (!isTRUE(ok)) {
    quit(status = 1)
  }
}

if (identical(environment(), globalenv()) && !length(sys.frames())) {
  tryCatch(main(), error = function(e) {
    message("Error: ", e$message)
    quit(status = 1)
  })
}
