#!/usr/bin/env Rscript
# ==============================================================================
# INTERACTIVE NETWORK PANELS (visNetwork HTML)
# ==============================================================================
#
# Produces interactive HTML network visualisations for each SAOM-aligned wave
# (2, 4, 5, 6) using visNetwork + igraph. Nodes are coloured by AUDIT-C score
# using a perceptually uniform, colourblind-safe palette (viridis-inspired),
# sized by total degree centrality, and display a styled tooltip on hover with
# actor ID, AUDIT-C, in-/out-degree, sex, and majority status.
#
# Outputs:
#   - One self-contained HTML per wave: network_wave2.html, etc.
#   - One combined tabbed HTML: network_all_waves.html
#   - All written to reproduced/outputs/chapter7/interactive_networks/
#
# Usage:
#   Rscript 05_interactive_networks.R [data_dir]
#
# Requirements: 4.1-4.8, 5.1, 5.2, 5.4, 5.5, 5.7
# ==============================================================================

# --- Source shared utilities --------------------------------------------------

local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("--file=", "", args[grepl("--file=", args)])
  if (length(file_arg) > 0) {
    script_dir <- dirname(file_arg)
  } else {
    script_dir <- "reproduced/scripts/visualisation"
    if (!dir.exists(script_dir)) script_dir <- "scripts/visualisation"
  }
  source(file.path(script_dir, "viz_utils.R"), local = globalenv())
})

# --- build_wave_graph ---------------------------------------------------------

build_wave_graph <- function(wave_df, all_ids) {
  if ("nomination" %in% names(wave_df)) {
    edges <- wave_df[!is.na(wave_df$nomination),
                     c("redcap_survey_identifier", "nomination")]
    edges <- edges[edges$nomination %in% all_ids, ]
    names(edges) <- c("from", "to")
  } else {
    edges <- data.frame(from = integer(0), to = integer(0))
  }

  g <- graph_from_data_frame(edges, directed = TRUE,
                             vertices = data.frame(name = all_ids))
  unique_rows <- wave_df[!duplicated(wave_df$redcap_survey_identifier), ]
  audit_lookup <- setNames(unique_rows$audit_score,
                           unique_rows$redcap_survey_identifier)
  V(g)$audit_score <- as.numeric(audit_lookup[as.character(V(g)$name)])
  V(g)$in_degree    <- degree(g, mode = "in")
  V(g)$out_degree   <- degree(g, mode = "out")
  V(g)$total_degree <- degree(g, mode = "all")
  g
}

# --- Package checks ----------------------------------------------------------

check_packages(
  c("igraph", "visNetwork", "htmlwidgets"),
  context = "05_interactive_networks.R"
)

suppressPackageStartupMessages({
  library(igraph)
  library(visNetwork)
  library(htmlwidgets)
})

# --- Colour design system -----------------------------------------------------
# Academic-grade palette now lives in viz_utils.R as audit_palette() and
# audit_border_palette(). Use format="rgba" for vis.js compatibility.

academic_audit_palette <- function(scores, max_score = 12) {
  audit_palette(scores, max_score = max_score, format = "rgba")
}

academic_audit_border <- function(scores, max_score = 12) {
  audit_border_palette(scores, max_score = max_score, format = "rgba")
}

# --- Typography & design tokens -----------------------------------------------

FONT_STACK <- "'Inter', 'Helvetica Neue', 'Segoe UI', system-ui, sans-serif"
COLOUR_TEXT_PRIMARY   <- "#2d3436"
COLOUR_TEXT_SECONDARY <- "#636e72"
COLOUR_TEXT_MUTED     <- "#b2bec3"
COLOUR_ACCENT         <- "#4a6fa5"
COLOUR_ACCENT_LIGHT   <- "#6c8ebf"
COLOUR_BG_CANVAS      <- "#fafbfc"
COLOUR_BG_CARD        <- "#ffffff"
COLOUR_BORDER_SUBTLE  <- "#dfe6e9"
COLOUR_EDGE_DEFAULT   <- "rgba(100, 110, 120, 0.10)"
COLOUR_EDGE_HIGHLIGHT <- "rgba(74, 111, 165, 0.55)"
COLOUR_EDGE_HOVER     <- "rgba(74, 111, 165, 0.35)"

# --- Core functions -----------------------------------------------------------

build_visnetwork_data <- function(graph, wave_label, node_scale = 1.0,
                                  data_mode = "real") {
  ids          <- V(graph)$name
  audit_scores <- V(graph)$audit_score
  in_deg       <- V(graph)$in_degree
  out_deg      <- V(graph)$out_degree
  total_deg    <- V(graph)$total_degree

  node_colours  <- academic_audit_palette(audit_scores)
  border_colours <- academic_audit_border(audit_scores)

  # Gentle sizing: log-scaled so high-degree nodes are visible but not dominant
  base_size <- 10
  node_sizes <- (base_size + sqrt(pmax(total_deg, 0)) * 3.5) * node_scale

  has_sex      <- "sex" %in% vertex_attr_names(graph)
  has_majority <- "majority_status" %in% vertex_attr_names(graph)
  sex_vals      <- if (has_sex) vertex_attr(graph, "sex") else rep(NA, length(ids))
  majority_vals <- if (has_majority) vertex_attr(graph, "majority_status") else rep(NA, length(ids))
  actor_label <- if (identical(data_mode, "proxy")) "Synthetic actor" else "Participant"

  tooltips <- mapply(function(id, audit, ind, outd, td, sx, maj) {
    audit_str <- if (is.na(audit)) "\u2014" else as.character(audit)
    risk_label <- if (is.na(audit)) "" else if (audit <= 3) {
      "<span style='color:#93b5a6;font-size:10px;'>\u25cf Low risk</span>"
    } else if (audit <= 7) {
      "<span style='color:#c4a35a;font-size:10px;'>\u25cf Increasing risk</span>"
    } else {
      "<span style='color:#8b4553;font-size:10px;'>\u25cf Higher risk</span>"
    }

    lines <- c(
      sprintf("<div style='font-family:%s;font-size:12px;line-height:1.7;padding:6px 4px;min-width:160px;'>", FONT_STACK),
      sprintf("<div style='font-size:13px;font-weight:600;color:%s;margin-bottom:4px;'>%s %s</div>", COLOUR_TEXT_PRIMARY, actor_label, id),
      sprintf("<div style='height:1px;background:%s;margin:4px 0 6px 0;'></div>", COLOUR_BORDER_SUBTLE),
      sprintf("<table style='border-collapse:collapse;width:100%%;font-size:11.5px;color:%s;'>", COLOUR_TEXT_SECONDARY),
      sprintf("<tr><td style='padding:1px 8px 1px 0;color:%s;'>AUDIT-C</td><td style='font-weight:600;color:%s;'>%s</td></tr>", COLOUR_TEXT_MUTED, COLOUR_TEXT_PRIMARY, audit_str),
      sprintf("<tr><td colspan='2' style='padding:0 0 2px 0;'>%s</td></tr>", risk_label),
      sprintf("<tr><td style='padding:1px 8px 1px 0;color:%s;'>Degree</td><td><b>%d</b> <span style='color:%s;'>(in %d, out %d)</span></td></tr>", COLOUR_TEXT_MUTED, td, COLOUR_TEXT_MUTED, ind, outd)
    )
    if (!is.na(sx))  lines <- c(lines, sprintf("<tr><td style='padding:1px 8px 1px 0;color:%s;'>Sex</td><td>%s</td></tr>", COLOUR_TEXT_MUTED, sx))
    if (!is.na(maj)) lines <- c(lines, sprintf("<tr><td style='padding:1px 8px 1px 0;color:%s;'>Majority</td><td>%s</td></tr>", COLOUR_TEXT_MUTED, maj))
    lines <- c(lines, "</table></div>")
    paste(lines, collapse = "")
  }, ids, audit_scores, in_deg, out_deg, total_deg, sex_vals, majority_vals,
  SIMPLIFY = TRUE, USE.NAMES = FALSE)

  # Node shape: circles with subtle shadow
  # NOTE: avoid color.highlight.border / color.hover.border columns —
  # visNetwork's dataFrameToD3 can't nest two-deep dot paths when the
  # intermediate key is already a scalar string.  Set highlight/hover
  # colours globally via visNodes() instead.
  nodes_df <- data.frame(
    id = ids,
    label = "",
    color = node_colours,
    color.border = border_colours,
    size = node_sizes,
    title = tooltips,
    shape = "dot",
    borderWidth = 1.2,
    borderWidthSelected = 2.5,
    shadow = TRUE,
    stringsAsFactors = FALSE
  )

  el <- as_edgelist(graph)
  if (nrow(el) > 0) {
    edges_df <- data.frame(
      from = el[, 1], to = el[, 2],
      arrows = "to",
      color = COLOUR_EDGE_DEFAULT,
      width = 0.5,
      smooth = TRUE,
      stringsAsFactors = FALSE
    )
  } else {
    edges_df <- data.frame(
      from = character(0), to = character(0), arrows = character(0),
      color = character(0), width = numeric(0), smooth = logical(0),
      stringsAsFactors = FALSE
    )
  }

  list(nodes_df = nodes_df, edges_df = edges_df)
}

# --- Wave label mapping -------------------------------------------------------

wave_time_labels <- c(
  "2" = "October 2022",
  "4" = "December 2022",
  "5" = "March 2023",
  "6" = "October 2023"
)

# --- Summary statistics -------------------------------------------------------

#' Compute summary statistics for a wave's igraph object.
#'
#' @param graph An igraph directed graph for a single wave.
#' @return A named list with density, reciprocity, avg_in_degree,
#'         avg_out_degree, n_participants, and n_nominations.
compute_wave_stats <- function(graph) {
  list(
    density        = igraph::graph.density(graph),
    reciprocity    = igraph::reciprocity(graph),
    avg_in_degree  = mean(igraph::degree(graph, mode = "in")),
    avg_out_degree = mean(igraph::degree(graph, mode = "out")),
    n_participants = igraph::vcount(graph),
    n_nominations  = igraph::ecount(graph)
  )
}

#' Build an HTML string for the summary statistics panel.
#'
#' @param stats Named list from compute_wave_stats().
#' @return A single HTML string suitable for htmlwidgets::appendContent().
build_stats_panel_html <- function(stats, data_mode = "real") {
  fmt <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
  actor_label <- if (identical(data_mode, "proxy")) "Synthetic Actors" else "Participants"
  sprintf(
    paste0(
      '<div style="',
      'font-family:%s;',
      'max-width:380px;',
      'margin:12px auto 0 auto;',
      'padding:14px 20px;',
      'background:%s;',
      'border:1px solid %s;',
      'border-radius:8px;',
      'box-shadow:0 1px 4px rgba(0,0,0,0.04);',
      '">',
      '<div style="font-size:12px;font-weight:600;color:%s;margin-bottom:8px;letter-spacing:0.3px;">',
      'Network Summary</div>',
      '<table style="width:100%%;border-collapse:collapse;font-size:11.5px;color:%s;">',
      '<tr><td style="padding:2px 0;">Density</td><td style="text-align:right;font-weight:500;">%s</td></tr>',
      '<tr><td style="padding:2px 0;">Reciprocity</td><td style="text-align:right;font-weight:500;">%s</td></tr>',
      '<tr><td style="padding:2px 0;">Avg In-Degree</td><td style="text-align:right;font-weight:500;">%s</td></tr>',
      '<tr><td style="padding:2px 0;">Avg Out-Degree</td><td style="text-align:right;font-weight:500;">%s</td></tr>',
      '<tr><td style="padding:2px 0;">%s</td><td style="text-align:right;font-weight:500;">%d</td></tr>',
      '<tr><td style="padding:2px 0;">Nominations</td><td style="text-align:right;font-weight:500;">%d</td></tr>',
      '</table></div>'
    ),
    FONT_STACK,
    COLOUR_BG_CARD,
    COLOUR_BORDER_SUBTLE,
    COLOUR_TEXT_PRIMARY,
    COLOUR_TEXT_SECONDARY,
    fmt(stats$density),
    fmt(stats$reciprocity),
    fmt(stats$avg_in_degree, 1),
    fmt(stats$avg_out_degree, 1),
    actor_label,
    stats$n_participants,
    stats$n_nominations
  )
}

#' Build an HTML string for the interpretive caption.
#'
#' @param caption Character string with the wave caption text.
#' @return A single HTML string suitable for htmlwidgets::appendContent().
build_caption_html <- function(caption) {
  sprintf(
    paste0(
      '<div style="',
      'font-family:%s;',
      'max-width:600px;',
      'margin:8px auto 4px auto;',
      'padding:10px 16px;',
      'font-size:11.5px;',
      'font-style:italic;',
      'color:%s;',
      'text-align:center;',
      'line-height:1.6;',
      '">%s</div>'
    ),
    FONT_STACK,
    COLOUR_TEXT_SECONDARY,
    caption
  )
}

detect_visualisation_data_mode <- function(data_dir) {
  proxy_markers <- file.path(
    data_dir,
    c(".realistic_proxy_data", ".chapter4_synthetic", "list_by_wave_schema.csv")
  )
  if (tolower(basename(normalizePath(data_dir, mustWork = FALSE))) == "proxy" ||
      any(file.exists(proxy_markers))) {
    return("proxy")
  }
  "real"
}

build_provenance_banner_html <- function(data_mode) {
  if (identical(data_mode, "proxy")) {
    return(paste0(
      "<div data-sand-data-mode=\"proxy\" style=\"font-family:", FONT_STACK,
      ";max-width:760px;margin:10px auto;padding:10px 14px;",
      "font-size:11.5px;line-height:1.5;color:#355c54;background:#eef7f4;",
      "border:1px solid #b8d8cf;border-radius:6px;text-align:center;\">",
      "<strong>SYNTHETIC PROXY DATA</strong> &mdash; generated deterministically ",
      "for software demonstration. No participant records or real network ties ",
      "are shown.</div>"
    ))
  }
  paste0(
    "<div data-sand-data-mode=\"real\" style=\"font-family:", FONT_STACK,
    ";max-width:760px;margin:10px auto;padding:10px 14px;",
    "font-size:11.5px;line-height:1.5;color:#7a2630;background:#fff1f2;",
    "border:2px solid #b42335;border-radius:6px;text-align:center;\">",
    "<strong>PROTECTED REAL DATA &mdash; DO NOT PUBLISH</strong></div>"
  )
}

render_wave_html <- function(nodes_df, edges_df, wave_label, wave_num,
                             output_path, physics = TRUE,
                             stats = NULL, caption = NULL,
                             data_mode = "real") {
  n_nodes <- nrow(nodes_df)
  n_edges <- nrow(edges_df)
  time_label <- wave_time_labels[as.character(wave_num)]
  if (is.null(time_label) || is.na(time_label)) time_label <- ""

  is_proxy <- identical(data_mode, "proxy")
  title_text <- if (is_proxy) paste0(wave_label, " — synthetic proxy") else wave_label
  subtitle_text <- if (is_proxy) {
    sprintf(
      "%s &nbsp;\u00b7&nbsp; %d synthetic actors &nbsp;\u00b7&nbsp; %d synthetic nominations",
      time_label, n_nodes, n_edges
    )
  } else {
    sprintf(
      "%s &nbsp;\u00b7&nbsp; %d participants &nbsp;\u00b7&nbsp; %d nominations",
      time_label, n_nodes, n_edges
    )
  }

  # Build colour-scale legend: 5 stops
  legend_scores <- c(0, 3, 6, 9, 12)
  legend_labels <- c("0 (none)", "3 (low)", "6 (moderate)", "9 (increasing)", "12 (high)")
  legend_colours <- academic_audit_palette(legend_scores)
  legend_nodes <- data.frame(
    label = legend_labels,
    shape = "dot",
    size = 10,
    color = legend_colours,
    font.size = 10,
    font.color = COLOUR_TEXT_SECONDARY,
    borderWidth = 0.8,
    stringsAsFactors = FALSE
  )

  # Degree-size legend
  size_legend <- data.frame(
    label = c("Low degree", "High degree"),
    shape = "dot",
    size = c(10, 26),
    color = "#b2bec3",
    font.size = 10,
    font.color = COLOUR_TEXT_SECONDARY,
    borderWidth = 0.8,
    stringsAsFactors = FALSE
  )
  legend_nodes <- rbind(legend_nodes, size_legend)

  vn <- visNetwork(nodes_df, edges_df,
                   main = list(
                     text = title_text,
                     style = sprintf(
                       "font-family:%s;font-size:18px;font-weight:600;color:%s;text-align:center;padding:16px 0 2px 0;letter-spacing:-0.3px;",
                       FONT_STACK, COLOUR_TEXT_PRIMARY
                     )
                   ),
                   submain = list(
                     text = subtitle_text,
                     style = sprintf(
                       "font-family:%s;font-size:11.5px;color:%s;text-align:center;padding-bottom:6px;letter-spacing:0.2px;",
                       FONT_STACK, COLOUR_TEXT_SECONDARY
                     )
                   ),
                   footer = list(
                     text = if (is_proxy) {
                       "Synthetic demonstration &nbsp;&nbsp;\u00b7&nbsp;&nbsp; Node colour = proxy AUDIT-C &nbsp;&nbsp;\u00b7&nbsp;&nbsp; Node size = degree centrality"
                     } else {
                       "PROTECTED REAL DATA — DO NOT PUBLISH &nbsp;&nbsp;\u00b7&nbsp;&nbsp; Node colour = AUDIT-C &nbsp;&nbsp;\u00b7&nbsp;&nbsp; Node size = degree centrality"
                     },
                     style = sprintf(
                       "font-family:%s;font-size:10px;color:%s;text-align:center;padding:8px 0 4px 0;",
                       FONT_STACK, COLOUR_TEXT_MUTED
                     )
                   ),
                   background = COLOUR_BG_CANVAS,
                   width = "100%", height = "780px") %>%
    visNodes(
      shadow = list(enabled = TRUE, size = 8, x = 1, y = 2,
                    color = "rgba(0,0,0,0.06)"),
      font = list(size = 0),
      color = list(
        highlight = list(background = COLOUR_ACCENT,
                         border = COLOUR_ACCENT),
        hover     = list(background = COLOUR_ACCENT_LIGHT,
                         border = COLOUR_ACCENT_LIGHT)
      )
    ) %>%
    visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.35,
                               type = "arrow")),
      color = list(
        color = COLOUR_EDGE_DEFAULT,
        highlight = COLOUR_EDGE_HIGHLIGHT,
        hover = COLOUR_EDGE_HOVER,
        opacity = 1.0
      ),
      smooth = list(enabled = TRUE, type = "continuous", roundness = 0.5),
      width = 0.5,
      hoverWidth = 0.6,
      selectionWidth = 1.2
    ) %>%
    visPhysics(
      enabled = physics,
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -35,
        centralGravity = 0.006,
        springLength = 100,
        springConstant = 0.05,
        damping = 0.45,
        avoidOverlap = 0.35
      ),
      stabilization = list(iterations = 400, fit = TRUE)
    ) %>%
    visInteraction(
      tooltipDelay = 80,
      hover = TRUE,
      navigationButtons = TRUE,
      keyboard = TRUE,
      zoomView = TRUE,
      dragView = TRUE,
      tooltipStyle = sprintf(
        "position:fixed;visibility:hidden;padding:10px 14px;font-family:%s;font-size:12px;color:%s;background:%s;border-radius:8px;border:1px solid %s;box-shadow:0 4px 16px rgba(0,0,0,0.08);pointer-events:none;max-width:260px;",
        FONT_STACK, COLOUR_TEXT_PRIMARY, COLOUR_BG_CARD, COLOUR_BORDER_SUBTLE
      )
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1,
                              hover = TRUE, algorithm = "all",
                              hideColor = "rgba(200,200,200,0.15)"),
      nodesIdSelection = list(
        enabled = FALSE
      )
    ) %>%
    visLegend(
      addNodes = legend_nodes,
      useGroups = FALSE,
      position = "right",
      width = 0.13,
      ncol = 1,
      main = list(
        text = "AUDIT-C Score",
        style = sprintf(
          "font-family:%s;font-size:11px;font-weight:600;color:%s;padding-bottom:4px;",
          FONT_STACK, COLOUR_TEXT_PRIMARY
        )
      )
    )

  # --- Append provenance, stats panel, and caption ---------------------------
  provenance_html <- htmltools::HTML(build_provenance_banner_html(data_mode))
  vn <- htmlwidgets::appendContent(vn, provenance_html)
  if (!is.null(stats)) {
    stats_html <- htmltools::HTML(build_stats_panel_html(stats, data_mode = data_mode))
    vn <- htmlwidgets::appendContent(vn, stats_html)
  }
  if (!is.null(caption) && nzchar(caption)) {
    caption_html <- htmltools::HTML(build_caption_html(caption))
    vn <- htmlwidgets::appendContent(vn, caption_html)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  out_abs <- normalizePath(output_path, mustWork = FALSE)
  htmlwidgets::saveWidget(vn, file = out_abs, selfcontained = TRUE)
  files_dir <- sub("\\.html$", "_files", out_abs)
  if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)
  invisible(NULL)
}

render_combined_html <- function(wave_files, wave_labels, wave_nums, output_path,
                                 captions = NULL, data_mode = "real") {
  tab_ids <- paste0("tab-", seq_along(wave_labels))

  buttons_html <- vapply(seq_along(wave_labels), function(i) {
    active <- if (i == 1) " active" else ""
    time_label <- wave_time_labels[as.character(wave_nums[i])]
    if (is.null(time_label) || is.na(time_label)) time_label <- ""
    sprintf(
      '<button class="tab-btn%s" onclick="switchTab(\'%s\', this)" title="%s">%s</button>',
      active, tab_ids[i], time_label, wave_labels[i]
    )
  }, character(1))

  panels_html <- vapply(seq_along(wave_labels), function(i) {
    display <- if (i == 1) "block" else "none"
    wn <- as.character(wave_nums[i])
    cap_text <- if (!is.null(captions) && !is.null(captions[[wn]]) && nzchar(captions[[wn]])) {
      captions[[wn]]
    } else {
      "Caption not configured for this wave."
    }
    caption_div <- sprintf(
      '<div style="font-family:%s;max-width:700px;margin:8px auto;padding:10px 16px;font-size:11.5px;font-style:italic;color:%s;text-align:center;line-height:1.6;">%s</div>',
      FONT_STACK, COLOUR_TEXT_SECONDARY, cap_text
    )
    sprintf(
      '<div id="%s" class="tab-panel" style="display:%s;"><iframe src="%s" style="width:100%%;height:800px;border:none;border-radius:0 0 8px 8px;"></iframe>%s</div>',
      tab_ids[i], display, wave_files[i], caption_div
    )
  }, character(1))

  tab_ids_json <- paste0("[", paste(sprintf('"%s"', tab_ids), collapse = ","), "]")

  html_content <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>Friendship Network Panels \u2014 SAOM Waves</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap" rel="stylesheet">
<style>
  *, *::before, *::after { box-sizing: border-box; }
  body {
    font-family: %s;
    margin: 0; padding: 0;
    background: #f5f6f8;
    color: %s;
    -webkit-font-smoothing: antialiased;
  }
  .container {
    max-width: 1400px;
    margin: 0 auto;
    padding: 32px 28px 20px;
  }
  .header {
    margin-bottom: 24px;
  }
  .header h1 {
    font-size: 22px;
    font-weight: 600;
    color: %s;
    margin: 0 0 6px 0;
    letter-spacing: -0.4px;
  }
  .header p {
    font-size: 13px;
    color: %s;
    margin: 0;
    line-height: 1.5;
  }
  .header .note {
    font-size: 11px;
    color: %s;
    margin-top: 8px;
    padding: 8px 12px;
    background: %s;
    border-radius: 6px;
    border-left: 3px solid %s;
  }
  .tab-bar {
    display: flex;
    gap: 2px;
    margin-bottom: 0;
    padding: 0;
  }
  .tab-btn {
    background: #edf0f2;
    border: 1px solid %s;
    border-bottom: none;
    padding: 10px 28px;
    cursor: pointer;
    font-family: %s;
    font-size: 13px;
    font-weight: 500;
    color: %s;
    border-radius: 8px 8px 0 0;
    transition: all 0.15s ease;
    outline: none;
    position: relative;
  }
  .tab-btn:hover {
    background: #e4e8eb;
    color: %s;
  }
  .tab-btn.active {
    background: %s;
    color: %s;
    font-weight: 600;
    border-color: %s;
    box-shadow: 0 -1px 3px rgba(0,0,0,0.04);
    z-index: 1;
  }
  .tab-panel {
    background: %s;
    border: 1px solid %s;
    border-top: none;
    border-radius: 0 0 8px 8px;
    box-shadow: 0 2px 12px rgba(0,0,0,0.04);
    overflow: hidden;
  }
</style>
</head>
<body>
<div class="container">
  <div class="header">
    <h1>Friendship Network Panels</h1>
    <p>Co-evolution of social networks and alcohol consumption \u2014 SAOM-aligned waves (2, 4, 5, 6). Node colour encodes AUDIT-C score; node size reflects degree centrality. Click any node to inspect individual attributes.</p>
    PROVENANCE_BANNER
  </div>
  <div class="tab-bar">
%s
  </div>
%s
</div>
<script>
function switchTab(tabId, btn) {
  var ids = %s;
  for (var i = 0; i < ids.length; i++) {
    document.getElementById(ids[i]).style.display = "none";
  }
  document.getElementById(tabId).style.display = "block";
  var btns = document.querySelectorAll(".tab-btn");
  for (var j = 0; j < btns.length; j++) {
    btns[j].classList.remove("active");
  }
  btn.classList.add("active");
}
</script>
</body>
</html>',
    # sprintf arguments in order:
    FONT_STACK,                    # body font-family
    COLOUR_TEXT_PRIMARY,           # body color
    COLOUR_TEXT_PRIMARY,           # h1 color
    COLOUR_TEXT_SECONDARY,         # p color
    COLOUR_TEXT_SECONDARY,         # .note color
    COLOUR_BG_CANVAS,             # .note background
    COLOUR_ACCENT_LIGHT,          # .note border-left
    COLOUR_BORDER_SUBTLE,         # .tab-btn border
    FONT_STACK,                    # .tab-btn font-family
    COLOUR_TEXT_SECONDARY,         # .tab-btn color
    COLOUR_TEXT_PRIMARY,           # .tab-btn:hover color
    COLOUR_BG_CARD,               # .tab-btn.active background
    COLOUR_TEXT_PRIMARY,           # .tab-btn.active color
    COLOUR_BORDER_SUBTLE,         # .tab-btn.active border-color
    COLOUR_BG_CARD,               # .tab-panel background
    COLOUR_BORDER_SUBTLE,         # .tab-panel border
    paste(buttons_html, collapse = "\n    "),
    paste(panels_html, collapse = "\n  "),
    tab_ids_json
  )

  html_content <- sub(
    "PROVENANCE_BANNER",
    build_provenance_banner_html(data_mode),
    html_content,
    fixed = TRUE
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(html_content, output_path)
  invisible(NULL)
}

# --- Main entry point ---------------------------------------------------------

main <- function() {
  repo_root <- resolve_repo_root()
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) >= 1) {
    data_dir <- normalizePath(args[1], mustWork = FALSE)
  } else {
    data_dir <- resolve_data_dir(repo_root)
  }
  data_mode <- detect_visualisation_data_mode(data_dir)

  viz_config  <- read_viz_config(repo_root)
  net_config  <- viz_config$interactive_network
  physics     <- isTRUE(net_config$physics)
  node_scale  <- net_config$node_scale
  wave_captions <- net_config$wave_captions

  output_dir <- file.path(repo_root, "outputs", "chapter7", "interactive_networks")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  message("[visnetwork] === Interactive Network Panels ===")
  message("[visnetwork] Data source: ", data_dir)
  message("[visnetwork] Data mode: ", data_mode)
  message("[visnetwork] Output directory: ", output_dir)
  message("[visnetwork] Physics layout: ", physics)
  message("[visnetwork] Node scale: ", node_scale)

  if (!dir.exists(data_dir)) {
    stop("Data directory not found: ", data_dir, call. = FALSE)
  }
  list_by_wave <- load_list_by_wave(data_dir)
  message(sprintf("[visnetwork] Loaded %d waves from list_by_wave.RData",
                  length(list_by_wave)))

  saom_wave_indices <- c(2, 4, 5, 6)
  wave_labels       <- c("Wave 2", "Wave 4", "Wave 5", "Wave 6")
  all_ids <- sort(unique(list_by_wave[[1]]$redcap_survey_identifier))
  message(sprintf("[visnetwork] Baseline participants: %d", length(all_ids)))

  # --- Replace source IDs with stable public-safe display labels --------------
  # HTML outputs must never contain source REDCap survey identifiers.
  # Build a stable mapping: source ID → sequential display label (P001, P002…).
  anon_map <- setNames(
    sprintf("P%03d", seq_along(all_ids)),
    as.character(all_ids)
  )
  anon_ids <- unname(anon_map)  # display IDs in same order as all_ids
  if (identical(data_mode, "proxy")) {
    message(sprintf("[visnetwork] Assigned %d synthetic actor labels (P001–P%03d)",
                    length(anon_ids), length(anon_ids)))
  } else {
    message(sprintf("[visnetwork] Replaced %d protected participant IDs with display labels (P001–P%03d)",
                    length(anon_ids), length(anon_ids)))
  }

  graphs <- lapply(seq_along(saom_wave_indices), function(i) {
    w <- saom_wave_indices[i]
    wave_df <- list_by_wave[[w]]
    # Remap IDs in the wave data before building the graph
    wave_df$redcap_survey_identifier <- anon_map[as.character(wave_df$redcap_survey_identifier)]
    if ("nomination" %in% names(wave_df)) {
      wave_df$nomination <- anon_map[as.character(wave_df$nomination)]
    }
    g <- build_wave_graph(wave_df, anon_ids)
    unique_rows <- wave_df[!duplicated(wave_df$redcap_survey_identifier), ]
    if ("sex" %in% names(unique_rows)) {
      sex_lookup <- setNames(unique_rows$sex, unique_rows$redcap_survey_identifier)
      V(g)$sex <- as.character(sex_lookup[as.character(V(g)$name)])
    }
    if ("majority_status" %in% names(unique_rows)) {
      maj_lookup <- setNames(unique_rows$majority_status,
                             unique_rows$redcap_survey_identifier)
      V(g)$majority_status <- as.character(maj_lookup[as.character(V(g)$name)])
    }
    g
  })

  wave_filenames <- character(length(graphs))
  for (i in seq_along(graphs)) {
    lbl <- wave_labels[i]
    message(sprintf("[visnetwork] Building visNetwork data for %s...", lbl))
    vn_data <- build_visnetwork_data(
      graphs[[i]], lbl, node_scale = node_scale, data_mode = data_mode
    )
    wave_num <- saom_wave_indices[i]
    filename <- sprintf("network_wave%d.html", wave_num)
    wave_filenames[i] <- filename
    out_file <- file.path(output_dir, filename)

    # Compute summary statistics for this wave
    wave_stats <- compute_wave_stats(graphs[[i]])

    # Look up interpretive caption from config
    wn_key <- as.character(wave_num)
    wave_caption <- if (!is.null(wave_captions) && !is.null(wave_captions[[wn_key]]) &&
                        nzchar(wave_captions[[wn_key]])) {
      wave_captions[[wn_key]]
    } else {
      "Caption not configured for this wave."
    }

    message(sprintf("[visnetwork] Rendering %s -> %s", lbl, out_file))
    render_wave_html(vn_data$nodes_df, vn_data$edges_df, lbl, wave_num,
                     out_file, physics = physics,
                     stats = wave_stats, caption = wave_caption,
                     data_mode = data_mode)
    message(sprintf("[visnetwork] Saved %s", out_file))
  }

  combined_path <- file.path(output_dir, "network_all_waves.html")
  message("[visnetwork] Rendering combined multi-wave HTML...")
  render_combined_html(wave_filenames, wave_labels, saom_wave_indices,
                       combined_path, captions = wave_captions,
                       data_mode = data_mode)
  message(sprintf("[visnetwork] Saved combined HTML: %s", combined_path))

  message("[visnetwork] === Done. All interactive network panels generated. ===")
}

if (identical(environment(), globalenv())) {
  tryCatch(main(), error = function(e) {
    message("[visnetwork] Error: ", e$message)
    quit(status = 1)
  })
}
