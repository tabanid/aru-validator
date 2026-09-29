# =============================================================================
# plot.R  —  Sidebar plots.
#   classification_plot_pY()  F1: p(Y) vs AI-score scatter + logistic fit
#                                 (lifted from the old tool's classification_plot)
#   temporal_tiles()          F2: compact unit x time_period status grid
#                                 (fixed grain — whatever the DB carries)
# =============================================================================

library(ggplot2)

# Where each code sits on the p(Y) scatter (y), its vertical jitter and point size.
# Keyed by the shared codes (db.R sources shared/codes.R); Y/N at 1/0, the other
# not-target codes just above 0, U just below 1.
PLOT_CODE_STYLE <- data.frame(
  y    = c(Y = 1,    N = 0,    O = 0.1,  I = 0.1,  P = 0.1,  U = 0.9),   # code-map
  yjit = c(Y = 0.03, N = 0.03, O = 0.02, I = 0.02, P = 0.02, U = 0.02),  # code-map
  cex  = c(Y = 1.2,  N = 1.2,  O = 1.0,  I = 1.0,  P = 1.0,  U = 0.9))   # code-map
stopifnot(setequal(rownames(PLOT_CODE_STYLE), ALL_CODES))
PLOT_CODE_ORDER <- c(DETECTION, NOT_TARGET, UNCERTAIN)   # draw order, as before

#' F1 — classification scatter with logistic-regression fit. `clips` needs
#' `score` and `classification`. Base graphics, matching the previous tool.
classification_plot_pY <- function(clips) {
  suppressWarnings({
    cl <- clips[!is.na(clips$classification) &
                  clips$classification %in% ALL_CODES,
                c("score", "classification"), drop = FALSE]

    if (nrow(cl) == 0) {
      par(mar = c(3, 3, 0.5, 1), mgp = c(1.8, 0.5, 0))
      plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1),
           xlab = "AI Score", ylab = "p(Y)", main = "", cex.axis = 0.7, cex.lab = 0.9)
      text(0.5, 0.5, "No classifications yet", cex = 1.2, col = "gray50")
      return(invisible())
    }

    n_yes     <- sum(cl$classification == DETECTION)
    n_not_yes <- sum(cl$classification %in% NOT_TARGET)

    score_range   <- range(cl$score, na.rm = TRUE)
    score_padding <- diff(score_range) * 0.05
    if (!is.finite(score_padding) || score_padding == 0) score_padding <- 0.05
    xlim_range    <- c(score_range[1] - score_padding, score_range[2] + score_padding)

    par(mar = c(3, 3, 0.5, 1), mgp = c(1.8, 0.5, 0))
    plot(1, type = "n", xlim = xlim_range, ylim = c(-0.1, 1.1),
         xlab = "AI Score", ylab = "p(Y)", main = "",
         yaxt = "n", cex.axis = 0.7, cex.lab = 0.9)
    axis(2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), cex.axis = 0.7)

    amt <- diff(xlim_range) * 0.01
    for (k in PLOT_CODE_ORDER) {
      sub <- cl[cl$classification == k, , drop = FALSE]
      if (nrow(sub) == 0) next
      st <- PLOT_CODE_STYLE[k, ]
      points(jitter(sub$score, amount = amt),
             jitter(rep(st$y, nrow(sub)), amount = st$yjit), pch = 16, cex = st$cex,
             col = adjustcolor(CODE_COLOUR[[k]], alpha.f = 0.6))
    }

    if (n_yes >= 4 && n_not_yes >= 4) {
      tryCatch({
        md       <- cl[cl$classification != UNCERTAIN, ]
        md$y_bin <- as.integer(md$classification == DETECTION)
        model    <- glm(y_bin ~ score, data = md, family = binomial)
        xs       <- seq(xlim_range[1], xlim_range[2], length.out = 100)
        ys       <- predict(model, newdata = data.frame(score = xs), type = "response")
        lines(xs, ys, col = "blue", lwd = 2)
      }, error = function(e) {})
    }
  })
}

#' Short x-axis label per unit (brief F3): the last 3 digits of the unit's trailing
#' number (Tobeatic-MSD00017 -> "017", MSD00079 -> "079"); 5 digits if 3 would clash;
#' the full name when a unit has no trailing number.
unit_short_labels <- function(units) {
  units <- as.character(units)
  digits <- sub("^.*?([0-9]+)$", "\\1", units, perl = TRUE)
  has <- grepl("[0-9]$", units)
  lab <- function(k) ifelse(has, substr(digits, pmax(1L, nchar(digits) - k + 1L), nchar(digits)), units)
  out <- lab(3L)
  if (anyDuplicated(out)) out <- lab(5L)
  if (anyDuplicated(out)) out <- units
  stats::setNames(out, units)
}

#' F2 — compact unit x time_period status tiles (fixed grain). `groups` is
#' derive_groups() output; `unit_order` orders columns west->east. `active` =
#' list(unit, period) of the cell on screen: drawn with a black outline (brief F3).
temporal_tiles <- function(groups, unit_order = NULL, active = NULL) {
  if (is.null(groups) || nrow(groups) == 0) return(.empty_plot("No data yet"))
  d <- data.frame(unit = as.character(groups$unit),
                  period = as.character(groups$time_period),
                  status = groups$status, stringsAsFactors = FALSE)
  units <- if (!is.null(unit_order)) unit_order else sort(unique(d$unit))
  d$unit   <- factor(d$unit, levels = units)
  d$period <- factor(d$period, levels = sort(unique(d$period)))
  d$status <- factor(d$status, levels = c("unexamined", "examined", "yes"))
  cols <- c(unexamined = "#cccccc", examined = "#dc3545", yes = "#28a745")

  n_periods <- nlevels(d$period)
  brk <- levels(d$period)
  if (n_periods > 10)
    brk <- levels(d$period)[seq(1, n_periods,
            by = if (n_periods <= 20) 2 else if (n_periods <= 50) 5 else 10)]

  p <- ggplot(d, aes(x = unit, y = period, fill = status)) +
    geom_tile(color = "#cccccc", linewidth = 0.5)
  if (!is.null(active) && !is.null(active$unit) && !is.null(active$period)) {
    a <- d[as.character(d$unit) == active$unit & as.character(d$period) == active$period, , drop = FALSE]
    if (nrow(a)) p <- p + geom_tile(data = a, fill = NA, color = "#000000", linewidth = 1.1)
  }
  p +
    scale_x_discrete(labels = unit_short_labels(units), drop = FALSE) +
    scale_y_discrete(limits = rev, breaks = brk) +
    scale_fill_manual(values = cols, guide = "none", drop = FALSE) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust = 1),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), axis.text.y = element_text(size = 8),
          axis.title.y = element_blank(), panel.grid = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
}

#' The (unit, period) under a click/hover on temporal_tiles, or NULL. `x`, `y` are the
#' plot's discrete positions as Shiny reports them; the axis order is read back from
#' the built plot, so it always matches what is drawn.
tile_at <- function(groups, unit_order, x, y) {
  if (is.null(groups) || nrow(groups) == 0 || is.null(x) || is.null(y)) return(NULL)
  pp <- ggplot_build(temporal_tiles(groups, unit_order))$layout$panel_params[[1]]
  xl <- pp$x$get_limits(); yl <- pp$y$get_limits()
  i <- round(x); j <- round(y)
  if (i < 1 || i > length(xl) || j < 1 || j > length(yl)) return(NULL)
  hit <- groups[as.character(groups$unit) == xl[i] & as.character(groups$time_period) == yl[j], , drop = FALSE]
  if (!nrow(hit)) return(NULL)                     # a white cell: no clip there
  list(unit = xl[i], period = yl[j])
}

.empty_plot <- function(msg) {
  ggplot() + annotate("text", x = 0, y = 0, label = msg, size = 4, color = "#666") +
    theme_void()
}
