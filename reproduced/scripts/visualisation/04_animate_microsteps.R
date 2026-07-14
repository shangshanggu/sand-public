#!/usr/bin/env Rscript
# ==============================================================================
# SAOM MICROSTEP ANIMATION — CHAIN-TO-NETWORKDYNAMIC CONVERTER & RENDERER
# ==============================================================================
#
# Converts extracted RSiena microstep chains (from 03_extract_chains.R) into
# networkDynamic objects and renders interactive HTML animations showing how
# the simulated network evolves one microstep at a time.
#
# Inspired by Adams & Schaefer (2018, Socius):
#   https://doi.org/10.1177/2378023118816545
#
# Usage:
#   Rscript reproduced/scripts/visualisation/04_animate_microsteps.R [--mp4] [--chain-index N]
#
# Defaults:
#   chain_index = thesis.yml visualisation.microstep.chain_index (1)
#   fps         = thesis.yml visualisation.microstep.fps (2)
#   node_scale  = thesis.yml visualisation.microstep.node_scale (1.5)
#
# Output:
#   reproduced/outputs/chapter7/microstep_animations/
#     microstep_wave2_to_wave4.html
#     microstep_wave4_to_wave5.html
#     microstep_wave5_to_wave6.html
#     (optionally .mp4 versions with --mp4)
#
# Requirements: 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 3.1, 3.2, 3.3, 3.4, 3.5,
#               3.6, 3.7, 5.2, 5.4, 5.7
# ==============================================================================

# --- Source shared utilities --------------------------------------------------

.script_dir <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("--file=", "", args[grepl("--file=", args)])
  if (length(file_arg) > 0) return(dirname(normalizePath(file_arg)))
  candidates <- c(
    "reproduced/scripts/visualisation",
    "scripts/visualisation"
  )
  for (d in candidates) {
    if (file.exists(file.path(d, "viz_utils.R"))) return(normalizePath(d))
  }
  "."
})()

source(file.path(.script_dir, "viz_utils.R"))

# --- Null-coalescing operator -------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# --- Constants ---------------------------------------------------------------

WAVE_TRANSITIONS <- list(
  list(label = "wave2_to_wave4", display = "W2->W4", period = 1L,
       from_wave = "Wave 2", to_wave = "Wave 4"),
  list(label = "wave4_to_wave5", display = "W4->W5", period = 2L,
       from_wave = "Wave 4", to_wave = "Wave 5"),
  list(label = "wave5_to_wave6", display = "W5->W6", period = 3L,
       from_wave = "Wave 5", to_wave = "Wave 6")
)

REQUIRED_PACKAGES <- c("network", "networkDynamic")
RENDER_PACKAGES   <- c("network", "networkDynamic", "ndtv")

# ==============================================================================
# COMPONENT 1: CHAIN-TO-NETWORKDYNAMIC CONVERTER
# ==============================================================================

#' Initialise a networkDynamic object from an observed wave-start state.
#'
#' Creates a directed networkDynamic with all n nodes active from [0, Inf),
#' edges from the adjacency matrix active from [0, Inf), and audit_score
#' vertex attribute set at t=0.
#'
#' @param adj_matrix  n x n binary adjacency matrix (0/1, zero diagonal).
#' @param audit_scores Numeric vector of length n with AUDIT-C scores (0-12).
#' @param node_ids    Character or integer vector of length n with node IDs.
#' @return A networkDynamic object.
init_network_dynamic <- function(adj_matrix, audit_scores, node_ids) {
  check_packages(REQUIRED_PACKAGES, context = "04_animate_microsteps.R")

  n <- nrow(adj_matrix)
  stopifnot(
    is.matrix(adj_matrix),
    ncol(adj_matrix) == n,
    length(audit_scores) == n,
    length(node_ids) == n
  )

  # Create a static network from the adjacency matrix
  adj_clean <- adj_matrix
  adj_clean[is.na(adj_clean)] <- 0L
  diag(adj_clean) <- 0L
  net <- network::network(adj_clean, directed = TRUE, loops = FALSE)

  # Set vertex names
  network::set.vertex.attribute(net, "vertex.names", as.character(node_ids))

  # Convert to networkDynamic — all nodes and existing edges active from [0, Inf)
  nd <- networkDynamic::networkDynamic(net, verbose = FALSE)

  # Activate all vertices for [0, Inf)
  networkDynamic::activate.vertices(nd, onset = 0, terminus = Inf, v = seq_len(n))

  # Activate existing edges for [0, Inf)
  existing_eids <- network::valid.eids(nd)
  if (length(existing_eids) > 0) {
    networkDynamic::activate.edges(nd, onset = 0, terminus = Inf, e = existing_eids)
  }

  # Set audit_score as a dynamic vertex attribute at t=0
  # Use activate.vertex.attribute for TEA (temporally extended attributes)
  networkDynamic::activate.vertex.attribute(
    nd, prefix = "audit_score",
    value = as.numeric(audit_scores),
    onset = 0, terminus = Inf,
    v = seq_len(n)
  )

  nd
}

#' Replay a microstep chain onto a networkDynamic object.
#'
#' For each microstep row in chain_df:
#'   - "change" (network): check current adjacency to determine create vs dissolve
#'     - If tie absent: activate edge (create) onset=t, terminus=Inf
#'     - If tie present: deactivate edge (dissolve) onset=t, terminus=t
#'   - "increase" (behaviour): increment audit_score for actor at time t
#'   - "decrease" (behaviour): decrement audit_score for actor at time t
#'   - "no_change": advance time counter, no state change
#'
#' A plain-R adjacency matrix and audit vector are maintained alongside the
#' networkDynamic to resolve "change" actions (create vs dissolve).
#'
#' @param nd           A networkDynamic object (from init_network_dynamic).
#' @param chain_df     Data frame with columns: step, actor, type, action, target.
#' @param initial_audit Numeric vector of initial AUDIT-C scores (length n).
#' @return The modified networkDynamic object (also modified in place).
replay_chain <- function(nd, chain_df, initial_audit) {
  check_packages(REQUIRED_PACKAGES, context = "04_animate_microsteps.R")

  n <- network::network.size(nd)
  m <- nrow(chain_df)

  if (m == 0L) {
    warning("[microstep] Empty chain (0 microsteps) — skipping replay.", call. = FALSE)
    return(nd)
  }

  # Maintain a plain-R adjacency matrix to track current state
  # (needed to determine create vs dissolve for "change" actions)
  cur_adj <- as.matrix(network::as.matrix.network(nd, matrix.type = "adjacency"))
  cur_adj[is.na(cur_adj)] <- 0L  # treat NA ties as absent
  cur_audit <- as.numeric(initial_audit)
  cur_audit[is.na(cur_audit)] <- 0

  t_current <- 1L  # time starts at 1 (t=0 is the initial state)

  for (i in seq_len(m)) {
    row <- chain_df[i, ]
    actor  <- as.integer(row$actor)
    action <- as.character(row$action)
    type   <- as.character(row$type)
    target <- if (!is.na(row$target)) as.integer(row$target) else NA_integer_

    if (identical(action, "no_change")) {
      # Advance time, no state change
      t_current <- t_current + 1L
      next
    }

    if (identical(type, "network") && identical(action, "change")) {
      # Network change: determine create vs dissolve from current adjacency
      if (is.na(target) || target < 1L || target > n || actor < 1L || actor > n) {
        t_current <- t_current + 1L
        next
      }

      if (cur_adj[actor, target] == 0) {
        # TIE CREATION: edge does not exist -> activate it
        # Check if edge already exists in the network object (may be inactive)
        eid <- network::get.edgeIDs(nd, v = actor, alter = target)
        if (length(eid) == 0) {
          # Add a new edge
          network::add.edge(nd, tail = actor, head = target)
          eid <- network::get.edgeIDs(nd, v = actor, alter = target)
        }
        # Activate the edge from t_current to Inf
        networkDynamic::activate.edges(nd, onset = t_current, terminus = Inf, e = eid)
        cur_adj[actor, target] <- 1L
      } else {
        # TIE DISSOLUTION: edge exists -> deactivate it
        eid <- network::get.edgeIDs(nd, v = actor, alter = target)
        if (length(eid) > 0) {
          networkDynamic::deactivate.edges(nd, onset = t_current, terminus = Inf, e = eid)
        }
        cur_adj[actor, target] <- 0L
      }
      t_current <- t_current + 1L
      next
    }

    if (identical(type, "behaviour")) {
      if (identical(action, "increase")) {
        cur_audit[actor] <- cur_audit[actor] + 1L
      } else if (identical(action, "decrease")) {
        cur_audit[actor] <- cur_audit[actor] - 1L
      }
      # Update the dynamic vertex attribute
      networkDynamic::activate.vertex.attribute(
        nd, prefix = "audit_score",
        value = cur_audit[actor],
        onset = t_current, terminus = Inf,
        v = actor
      )
      t_current <- t_current + 1L
      next
    }

    # Fallback: unrecognised action, just advance time
    t_current <- t_current + 1L
  }

  nd
}

#' Extract the final network state from a networkDynamic at a given time step.
#'
#' Reads back the adjacency matrix and audit scores at the specified time,
#' enabling round-trip validation against the expected final state.
#'
#' @param nd          A networkDynamic object.
#' @param total_steps Total number of microsteps (final time = total_steps).
#' @return A list with components:
#'   \item{adj_matrix}{n x n binary adjacency matrix at final time.}
#'   \item{audit_scores}{Numeric vector of AUDIT-C scores at final time.}
extract_final_state <- function(nd, total_steps) {
  check_packages(REQUIRED_PACKAGES, context = "04_animate_microsteps.R")

  n <- network::network.size(nd)

  # Extract the network at the final time step
  net_at_t <- networkDynamic::network.extract(nd, at = total_steps)

  # Get adjacency matrix (unname to drop dimnames for clean comparison)
  adj <- unname(
    as.matrix(network::as.matrix.network(net_at_t, matrix.type = "adjacency"))
  )

  # Get audit scores at final time step using bulk TEA query.
  # get.vertex.attribute.active() without v= returns a vector indexed by vertex.
  raw_scores <- networkDynamic::get.vertex.attribute.active(
    nd, prefix = "audit_score", at = total_steps
  )
  audit_scores <- if (is.null(raw_scores) || length(raw_scores) == 0) {
    rep(0, n)
  } else {
    ifelse(is.na(raw_scores), 0, as.numeric(raw_scores))
  }

  list(adj_matrix = adj, audit_scores = audit_scores)
}

# ==============================================================================
# COMPONENT 2: ANIMATION RENDERER
# ==============================================================================

#' Render an interactive HTML animation of the microstep process.
#'
#' Uses ndtv::render.d3movie() to produce a self-contained HTML file with:
#'   - Node colour: AUDIT-C academic palette (teal-to-wine via audit_palette())
#'   - Node size: compact, slightly scaled by AUDIT-C score
#'   - Stable Kamada-Kawai layout with isolates scattered to periphery
#'   - Post-render legend injection for AUDIT-C colour scale
#'
#' @param nd          A networkDynamic object with replayed chain.
#' @param layout      Unused (layout computed internally via compute.animation).
#' @param output_path File path for the output HTML file.
#' @param fps         Frames per second for the animation (default 2).
#' @param node_scale  Node size multiplier (default 1.5).
#' @param transition_label Human-readable label for the wave transition
#'                         (e.g. "Wave 2 \u2192 Wave 4"). Used in the legend.
#' @param n_microsteps Number of microsteps in this period's chain (for legend).
#' @return NULL (side effect: writes HTML file).
render_html_animation <- function(nd, layout, output_path, fps = 2,
                                  node_scale = 1.5, transition_label = "",
                                  n_microsteps = 0L) {
  check_packages(RENDER_PACKAGES, context = "04_animate_microsteps.R")

  n <- network::network.size(nd)

  # Compute the time range for the animation
  # Use the network's activity range
  time_range <- range(networkDynamic::get.change.times(nd))
  if (length(time_range) == 0 || all(is.na(time_range))) {
    time_range <- c(0, 1)
  }

  # Subsample: aim for ~100 frames max to keep the HTML manageable
  total_span <- time_range[2] - time_range[1]
  target_frames <- 100L
  slice_interval <- max(1L, as.integer(ceiling(total_span / target_frames)))

  # Ensure output directory exists
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  message(sprintf("[microstep] Rendering HTML animation to %s ...", output_path))
  message(sprintf("[microstep] Time range: [%d, %d], slice interval: %d (%d frames)",
                  time_range[1], time_range[2], slice_interval,
                  as.integer(ceiling(total_span / slice_interval))))

  # Compute the animation layout using Kamada-Kawai
  ndtv::compute.animation(
    nd,
    animation.mode = "kamadakawai",
    slice.par = list(
      start = time_range[1],
      end = time_range[2],
      interval = slice_interval,
      aggregate.dur = slice_interval,
      rule = "any"
    ),
    default.dist = 1,
    verbose = FALSE
  )

  # --- Scatter isolates away from the origin ---------------------------------
  # Kamada-Kawai places disconnected nodes at (0,0). Detect nodes clustered
  # near the origin and redistribute them in a ring around the main component
  # so they don't form a distracting blob in the centre.
  .scatter_origin_nodes <- function(nd) {
    # Get the stored animation coordinates (list of matrices, one per slice)
    coords_list <- network::get.vertex.attribute(nd, "animation.x.active")
    if (is.null(coords_list)) return(nd)  # no coords stored

    y_list <- network::get.vertex.attribute(nd, "animation.y.active")
    if (is.null(y_list)) return(nd)

    # Work on the first slice to find the layout extent
    # animation.x.active is a TEA — extract all unique coordinate sets
    # For simplicity, modify the network attribute directly via the
    # dynamic extension spells. We'll use a simpler approach: get all
    # x/y coords at time 0, find the bounding box, then offset origin nodes.

    # Extract coordinates at the first time point
    x0 <- networkDynamic::get.vertex.attribute.active(nd, "animation.x", at = time_range[1])
    y0 <- networkDynamic::get.vertex.attribute.active(nd, "animation.y", at = time_range[1])
    if (is.null(x0) || is.null(y0)) return(nd)

    x0[is.na(x0)] <- 0; y0[is.na(y0)] <- 0

    # Find nodes very close to origin (within 2% of layout range)
    x_range <- diff(range(x0))
    y_range <- diff(range(y0))
    layout_range <- max(x_range, y_range, 1)
    threshold <- layout_range * 0.02
    near_origin <- which(abs(x0) < threshold & abs(y0) < threshold)

    if (length(near_origin) <= 1) return(nd)  # 0 or 1 node at origin is fine

    # Place them in a ring at ~110% of the layout radius
    radius <- layout_range * 0.55
    angles <- seq(0, 2 * pi, length.out = length(near_origin) + 1)[-(length(near_origin) + 1)]
    set.seed(42)
    angles <- angles + runif(length(near_origin), -0.15, 0.15)  # jitter

    # Update coordinates for ALL time slices
    change_times <- networkDynamic::get.change.times(nd)
    if (length(change_times) == 0) change_times <- time_range[1]

    for (k in seq_along(near_origin)) {
      v <- near_origin[k]
      new_x <- radius * cos(angles[k])
      new_y <- radius * sin(angles[k])
      networkDynamic::activate.vertex.attribute(
        nd, "animation.x", value = new_x,
        onset = -Inf, terminus = Inf, v = v
      )
      networkDynamic::activate.vertex.attribute(
        nd, "animation.y", value = new_y,
        onset = -Inf, terminus = Inf, v = v
      )
    }

    message(sprintf("[microstep] Scattered %d origin-clustered nodes to periphery ring.",
                    length(near_origin)))
    nd
  }

  nd <- .scatter_origin_nodes(nd)

  # Render the d3 movie with static node colours based on initial AUDIT scores
  n <- network::network.size(nd)

  # Get initial audit scores for static colouring
  initial_scores <- networkDynamic::get.vertex.attribute.active(
    nd, prefix = "audit_score", at = 0
  )
  if (is.null(initial_scores) || all(is.na(initial_scores))) {
    initial_scores <- rep(0, n)
  }
  initial_scores[is.na(initial_scores)] <- 0

  static_colours <- audit_palette(initial_scores)
  border_colours <- audit_border_palette(initial_scores)

  # Node sizing: small and uniform-ish so edges are visible.
  # Base 0.6 + tiny AUDIT contribution keeps nodes compact for 248-node network.
  static_sizes <- (0.6 + initial_scores * 0.04) * node_scale

  ndtv::render.d3movie(
    nd,
    filename = output_path,
    render.par = list(
      tween.frames = 3,
      show.time = TRUE,
      show.stats = NULL
    ),
    plot.par = list(bg = "white"),
    d3.options = list(
      animationDuration = as.integer(1000 / fps),
      enterExitAnimationFactor = 0
    ),
    vertex.col = static_colours,
    vertex.cex = static_sizes,
    vertex.border = border_colours,
    vertex.lwd = 0.4,
    vertex.tooltip = paste0("Node ", seq_len(n), " | AUDIT-C: ", initial_scores),
    edge.col = grDevices::adjustcolor("#4a5568", alpha.f = 0.25),
    edge.lwd = 0.6,
    launchBrowser = FALSE,
    output.mode = "HTML",
    verbose = FALSE
  )

  # --- Inject legend into the HTML -------------------------------------------
  # ndtv's d3movie HTML has no built-in legend support. We inject a styled
  # overlay div just before the closing </body> tag.
  .inject_legend <- function(html_path, transition_label, n_nodes, n_steps) {
    html <- readLines(html_path, warn = FALSE)

    # Build colour swatches for the legend
    legend_scores <- c(0, 3, 6, 9, 12)
    legend_hex <- audit_palette(legend_scores)
    swatches <- vapply(seq_along(legend_scores), function(i) {
      sprintf(
        '<span style="display:inline-block;width:14px;height:14px;border-radius:3px;background:%s;margin-right:3px;vertical-align:middle;"></span><span style="vertical-align:middle;margin-right:10px;">%d</span>',
        legend_hex[i], legend_scores[i]
      )
    }, character(1))

    legend_html <- sprintf(
      '<div id="microstep-legend" style="position:fixed;top:12px;left:12px;z-index:9999;background:rgba(255,255,255,0.94);border:1px solid #dfe6e9;border-radius:8px;padding:14px 18px;font-family:Inter,Helvetica Neue,system-ui,sans-serif;font-size:11.5px;color:#2d3436;box-shadow:0 2px 12px rgba(0,0,0,0.07);max-width:260px;line-height:1.6;">
  <div style="font-weight:600;font-size:13px;margin-bottom:6px;">%s</div>
  <div style="height:1px;background:#dfe6e9;margin:4px 0 8px;"></div>
  <div style="margin-bottom:6px;"><span style="color:#636e72;">Nodes:</span> %d participants</div>
  <div style="margin-bottom:6px;"><span style="color:#636e72;">Microsteps:</span> %s</div>
  <div style="margin-bottom:8px;"><span style="color:#636e72;">Node colour:</span> AUDIT-C score</div>
  <div style="margin-bottom:2px;">%s</div>
  <div style="font-size:10px;color:#b2bec3;margin-top:6px;">Low risk \u2192 Higher risk</div>
</div>',
      if (nzchar(transition_label)) transition_label else "SAOM Microstep Animation",
      n_nodes,
      format(n_steps, big.mark = ","),
      paste(swatches, collapse = "")
    )

    # Insert before </body> — ndtv puts </script></body> on one line,
    # so we need to split that and insert between them
    body_close <- grep("</body>", html, fixed = TRUE)
    if (length(body_close) > 0) {
      insert_at <- body_close[1]
      # Replace the line containing </body> by splitting </script> from </body>
      original_line <- html[insert_at]
      # Insert legend HTML between </script> and </body>
      new_line <- sub("</body>", paste0("\n", legend_html, "\n</body>"), original_line, fixed = TRUE)
      # Also ensure the legend is outside any <script> block
      new_line <- sub("</script>", "</script>\n", new_line, fixed = TRUE)
      html[insert_at] <- new_line
    }

    writeLines(html, html_path)
  }

  .inject_legend(output_path, transition_label, n, n_microsteps)

  message(sprintf("[microstep] HTML animation saved: %s", output_path))
  invisible(NULL)
}

#' Render an mp4 video animation of the microstep process.
#'
#' Uses ndtv::saveVideo() which requires ffmpeg to be installed.
#' If ffmpeg is not found, issues a warning and returns without error.
#'
#' @param nd          A networkDynamic object with replayed chain.
#' @param layout      Layout matrix (n x 2) from compute_animation_layout().
#' @param output_path File path for the output mp4 file.
#' @param fps         Frames per second for the video (default 2).
#' @param node_scale  Node size multiplier (default 1.5).
#' @return NULL (side effect: writes mp4 file if ffmpeg available).
render_mp4_animation <- function(nd, layout, output_path, fps = 2, node_scale = 1.5) {
  check_packages(RENDER_PACKAGES, context = "04_animate_microsteps.R")

  # Check for ffmpeg
  ffmpeg_check <- tryCatch(
    system2("ffmpeg", args = "-version", stdout = TRUE, stderr = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  if (is.null(ffmpeg_check)) {
    warning(
      "[microstep] ffmpeg not found — mp4 rendering skipped. ",
      "Install ffmpeg to enable mp4 output.",
      call. = FALSE
    )
    return(invisible(NULL))
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  message(sprintf("[microstep] Rendering mp4 animation to %s ...", output_path))

  n <- network::network.size(nd)
  time_range <- range(networkDynamic::get.change.times(nd))
  if (length(time_range) == 0 || all(is.na(time_range))) {
    time_range <- c(0, 1)
  }

  # Compute animation if not already done
  total_span <- time_range[2] - time_range[1]
  slice_interval <- max(1L, as.integer(ceiling(total_span / 100)))

  ndtv::compute.animation(
    nd,
    animation.mode = "kamadakawai",
    slice.par = list(
      start = time_range[1],
      end = time_range[2],
      interval = slice_interval,
      aggregate.dur = slice_interval,
      rule = "any"
    ),
    default.dist = 1,
    verbose = FALSE
  )

  tryCatch({
    ndtv::saveVideo(
      ndtv::render.animation(
        nd,
        vertex.col = function(slice) {
          scores <- networkDynamic::get.vertex.attribute.active(
            slice, "audit_score",
            at = networkDynamic::get.change.times(slice)[1],
            require.active = FALSE
          )
          if (is.null(scores)) scores <- rep(0, network::network.size(slice))
          scores[is.na(scores)] <- 0
          audit_palette(scores)
        },
        vertex.cex = function(slice) {
          scores <- networkDynamic::get.vertex.attribute.active(
            slice, "audit_score",
            at = networkDynamic::get.change.times(slice)[1],
            require.active = FALSE
          )
          if (is.null(scores)) scores <- rep(0, network::network.size(slice))
          scores[is.na(scores)] <- 0
          (2 + scores * 0.5) * node_scale
        },
        edge.col = grDevices::adjustcolor("grey50", alpha.f = 0.5),
        edge.lwd = 0.5,
        verbose = FALSE
      ),
      filename = output_path,
      ani.width = 800,
      ani.height = 800
    )
    message(sprintf("[microstep] mp4 animation saved: %s", output_path))
  }, error = function(e) {
    warning(
      sprintf("[microstep] mp4 rendering failed: %s", conditionMessage(e)),
      call. = FALSE
    )
  })

  invisible(NULL)
}

# ==============================================================================
# CLI ARGUMENT PARSING
# ==============================================================================

#' Parse command-line arguments for the animation script.
#'
#' Supports:
#'   --mp4            Also render mp4 video (requires ffmpeg)
#'   --chain-index N  Which chain to animate (1-indexed, default 1)
#'
#' @return A list with $mp4 (logical) and $chain_index (integer or NULL).
parse_animate_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  result <- list(mp4 = FALSE, chain_index = NULL)

  if (length(args) == 0) return(result)

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]

    if (identical(arg, "--mp4")) {
      result$mp4 <- TRUE
      i <- i + 1L
      next
    }

    if (arg %in% c("--chain-index", "--chain_index")) {
      if (i >= length(args)) {
        stop("--chain-index flag provided without a value.", call. = FALSE)
      }
      val <- suppressWarnings(as.integer(args[[i + 1L]]))
      if (is.na(val)) {
        stop(
          sprintf("Invalid --chain-index value: '%s' (must be an integer).", args[[i + 1L]]),
          call. = FALSE
        )
      }
      result$chain_index <- val
      i <- i + 2L
      next
    }

    if (grepl("^--chain-index=", arg) || grepl("^--chain_index=", arg)) {
      val_str <- sub("^--chain[-_]index=", "", arg)
      val <- suppressWarnings(as.integer(val_str))
      if (is.na(val)) {
        stop(
          sprintf("Invalid --chain-index value: '%s' (must be an integer).", val_str),
          call. = FALSE
        )
      }
      result$chain_index <- val
      i <- i + 1L
      next
    }

    # Unknown argument
    stop(sprintf("Unrecognised argument: '%s'", arg), call. = FALSE)
  }

  result
}

# ==============================================================================
# MAIN
# ==============================================================================

main <- function() {
  # --- Setup -----------------------------------------------------------------
  repo_root <- resolve_repo_root()

  check_packages(REQUIRED_PACKAGES, context = "04_animate_microsteps.R")
  check_packages("yaml", context = "04_animate_microsteps.R")

  config <- yaml::read_yaml(file.path(repo_root, "config", "thesis.yml"))
  viz_config <- read_viz_config(repo_root, config)
  chapter_cfg <- config$chapters$chapter7_saom

  if (is.null(chapter_cfg)) {
    stop("chapter7_saom configuration block is missing from thesis.yml", call. = FALSE)
  }

  # --- Parse CLI args --------------------------------------------------------
  cli_args <- parse_animate_args()

  # Resolve chain_index: config -> CLI -> default
  chain_index <- viz_config$microstep$chain_index %||% 1L
  if (!is.null(cli_args$chain_index)) {
    chain_index <- cli_args$chain_index
  }
  chain_index <- as.integer(chain_index)

  # Resolve output format
  output_format <- viz_config$microstep$output_format %||% "html"
  render_mp4 <- cli_args$mp4 || identical(output_format, "both")

  # Animation parameters
  fps <- as.integer(viz_config$microstep$fps %||% 2L)
  node_scale <- as.numeric(viz_config$microstep$node_scale %||% 1.5)

  # --- Resolve paths ---------------------------------------------------------
  # Microstep chains (from 03_extract_chains.R)
  chains_path <- file.path(repo_root, "outputs", "chapter7", "microstep_chains.rds")
  if (!file.exists(chains_path)) {
    stop(
      sprintf(
        "Microstep chains not found at: %s\n  Run 03_extract_chains.R first to extract chains.",
        chains_path
      ),
      call. = FALSE
    )
  }

  # Network arrays (for initial adjacency + AUDIT scores)
  network_path <- NULL
  for (p in chapter_cfg$required_inputs) {
    if (grepl("network_arrays\\.rds$", p)) {
      network_path <- p
      break
    }
  }
  if (is.null(network_path)) {
    stop("chapter7_saom.required_inputs must include network_arrays.rds path.", call. = FALSE)
  }
  network_path_clean <- sub("^reproduced/", "", network_path)
  network_path_abs <- file.path(repo_root, network_path_clean)

  if (!file.exists(network_path_abs)) {
    stop(
      sprintf(
        "Network arrays not found at: %s\n  Run the Chapter 4 pipeline first.",
        network_path_abs
      ),
      call. = FALSE
    )
  }

  # Output directory
  output_dir <- file.path(repo_root, "outputs", "chapter7", "microstep_animations")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # --- Log configuration -----------------------------------------------------
  message("[microstep] === SAOM Microstep Animation ===")
  message(sprintf("[microstep] Chain index: %d", chain_index))
  message(sprintf("[microstep] FPS: %d", fps))
  message(sprintf("[microstep] Node scale: %.1f", node_scale))
  message(sprintf("[microstep] Output format: %s", if (render_mp4) "html + mp4" else "html"))
  message(sprintf("[microstep] Chains file: %s", chains_path))
  message(sprintf("[microstep] Network arrays: %s", network_path_abs))
  message(sprintf("[microstep] Output directory: %s", output_dir))

  # --- Load data -------------------------------------------------------------
  message("[microstep] Loading microstep chains ...")
  chain_data <- readRDS(chains_path)
  chains_df <- chain_data$chains
  metadata  <- chain_data$metadata

  message(sprintf(
    "[microstep] Loaded %d microsteps from %d chains across %d periods.",
    nrow(chains_df),
    metadata$n_chains %||% length(unique(chains_df$chain_id)),
    length(unique(chains_df$period))
  ))

  # Validate chain_index
  available_chains <- sort(unique(chains_df$chain_id))
  if (!chain_index %in% available_chains) {
    stop(
      sprintf(
        "Chain index %d not found. Available chains: %s",
        chain_index,
        paste(available_chains, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  message("[microstep] Loading network arrays ...")
  payload <- readRDS(network_path_abs)

  # network_arrays.rds has 4 waves (wave1=W2, wave2=W4, wave3=W5, wave4=W6)
  net_array <- payload$network_array    # 3D array: n x n x 4
  beh_array <- payload$behaviour_array  # matrix: n x 4

  n <- dim(net_array)[1]
  node_ids <- as.character(seq_len(n))
  message(sprintf("[microstep] Network size: %d nodes, %d waves.", n, dim(net_array)[3]))

  # Check for rendering packages (warn early but don't stop)
  render_available <- all(vapply(
    RENDER_PACKAGES,
    function(pkg) requireNamespace(pkg, quietly = TRUE),
    logical(1)
  ))
  if (!render_available) {
    message("[microstep] Note: ndtv package not available — will build networkDynamic objects but skip HTML rendering.")
    message("[microstep] Install ndtv for full animation: install.packages('ndtv')")
  }

  # --- Process each wave transition ------------------------------------------
  for (wt in WAVE_TRANSITIONS) {
    period <- wt$period
    label  <- wt$label
    display <- wt$display

    message(sprintf("\n[microstep] --- Processing %s (period %d) ---", display, period))

    # Filter chain for this period and chain_index
    period_chain <- chains_df[
      chains_df$chain_id == chain_index & chains_df$period == period,
    ]

    if (nrow(period_chain) == 0) {
      warning(
        sprintf("[microstep] No microsteps found for chain %d, period %d (%s) — skipping.",
                chain_index, period, display),
        call. = FALSE
      )
      next
    }

    message(sprintf("[microstep] Chain %d, period %d: %d microsteps.",
                    chain_index, period, nrow(period_chain)))

    # Get initial state for this wave transition
    # Period 1 = wave1->wave2 in array (W2->W4), so start from wave index = period
    wave_start_idx <- period  # 1-indexed into the 4-wave array
    adj_start <- net_array[, , wave_start_idx]
    audit_start <- beh_array[, wave_start_idx]

    # Replace NA audit scores with 0
    audit_start[is.na(audit_start)] <- 0

    message(sprintf("[microstep] Initial state: %d edges, mean AUDIT=%.1f",
                    sum(adj_start, na.rm = TRUE),
                    mean(audit_start, na.rm = TRUE)))

    # --- Build networkDynamic ------------------------------------------------
    message("[microstep] Initialising networkDynamic ...")
    nd <- init_network_dynamic(adj_start, audit_start, node_ids)

    # --- Replay chain --------------------------------------------------------
    message("[microstep] Replaying microstep chain ...")
    nd <- replay_chain(nd, period_chain, audit_start)

    # --- Extract and log final state -----------------------------------------
    total_steps <- nrow(period_chain)
    final_state <- extract_final_state(nd, total_steps)
    message(sprintf("[microstep] Final state: %d edges, mean AUDIT=%.1f",
                    sum(final_state$adj_matrix, na.rm = TRUE),
                    mean(final_state$audit_scores, na.rm = TRUE)))

    # --- Render animations ---------------------------------------------------
    if (render_available) {
      # Load ndtv (and its dependencies sna, animation) into the search path
      # so that compute.animation can find layout functions like kamadakawai
      suppressPackageStartupMessages({
        library(ndtv)
      })

      # HTML animation
      html_path <- file.path(output_dir, paste0("microstep_", label, ".html"))
      trans_label <- paste0(wt$from_wave, " \u2192 ", wt$to_wave)
      render_html_animation(nd, NULL, html_path, fps = fps,
                            node_scale = node_scale,
                            transition_label = trans_label,
                            n_microsteps = nrow(period_chain))

      # Optional mp4
      if (render_mp4) {
        mp4_path <- file.path(output_dir, paste0("microstep_", label, ".mp4"))
        render_mp4_animation(nd, NULL, mp4_path, fps = fps, node_scale = node_scale)
      }
    } else {
      message("[microstep] Skipping rendering (ndtv not available).")
    }

    message(sprintf("[microstep] --- %s complete ---", display))
  }

  # --- Summary ---------------------------------------------------------------
  message("\n[microstep] === Animation pipeline complete ===")
  output_files <- list.files(output_dir, full.names = FALSE)
  if (length(output_files) > 0) {
    message(sprintf("[microstep] Output files in %s:", output_dir))
    for (f in output_files) {
      message(sprintf("[microstep]   %s", f))
    }
  } else {
    message("[microstep] No output files generated (check warnings above).")
  }

  invisible(NULL)
}

# --- Entry point (guarded) ---------------------------------------------------

if (identical(environment(), globalenv())) {
  tryCatch(
    main(),
    error = function(err) {
      message(sprintf("[microstep] Error: %s", conditionMessage(err)))
      quit(status = 1)
    }
  )
}
