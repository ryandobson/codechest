# ============================================================
# anova_describe.R
#
# Layer 5: means, figures, effect sizes, and the sentence.
#
# Nothing here tests an assumption. These functions describe and
# report a model that has already been fitted.
# ============================================================


# ---- means, standard errors, intervals -------------------------------------

#' Group means with model-based standard errors and confidence intervals
#'
#' By default the standard error is the model-based one, `sqrt(MS_W / n_j)`,
#' with the model's error degrees of freedom. That is the standard error
#' consistent with the F test being reported, so error bars built from it match
#' the model. Error bars built from each group's own SD do not.
#'
#' `pooled = FALSE` gives each group its own SD and its own `n_j - 1` df, which
#' is three separate one-sample intervals drawn on shared axes. Use it when
#' homogeneity is violated, or when the figure is meant to describe each group
#' on its own terms without leaning on a model.
#'
#' @param model A fitted `aov()` or `lm()`.
#' @param conf Confidence level. Default `0.95`.
#' @param pooled Use the model's pooled error term. Default `TRUE`.
#'
#' @return A data.frame, one row per group, with `n`, `mean`, `sd`, `se`,
#'   `ci_low`, `ci_high`. The `se_type` attribute records which SE was used.
#'
#' @examples
#' fit <- aov(weight ~ group, data = PlantGrowth)
#' anova_means(fit)
#' anova_means(fit, pooled = FALSE)
#'
#' @seealso [anova_ref()] with `"se_mean"` or `"ms_within"` for the reasoning.
#' @importFrom stats qt sd
#' @export
anova_means <- function(model, conf = 0.95, pooled = TRUE) {

  p  <- .model_parts(model)
  er <- .error_term(model)

  out <- data.frame(
    group = levels(p$g),
    n     = as.vector(tapply(p$y, p$g, length)),
    mean  = as.vector(tapply(p$y, p$g, mean)),
    sd    = as.vector(tapply(p$y, p$g, stats::sd)),
    stringsAsFactors = FALSE
  )
  names(out)[1] <- p$g_name

  if (pooled) {
    out$se <- sqrt(er$MS_W / out$n)
    tcrit  <- stats::qt(1 - (1 - conf) / 2, df = er$df_W)
  } else {
    out$se <- out$sd / sqrt(out$n)
    tcrit  <- stats::qt(1 - (1 - conf) / 2, df = out$n - 1)
  }

  out$ci_low  <- out$mean - tcrit * out$se
  out$ci_high <- out$mean + tcrit * out$se

  attr(out, "se_type") <- if (pooled) "pooled, sqrt(MS_W / n)" else "per group, sd / sqrt(n)"
  attr(out, "conf")    <- conf
  out
}



# ---- theme handling --------------------------------------------------------

#' Resolve the `theme` argument of the anova_* plot functions.
#'
#' Accepts a shorthand name, a theme-returning function, or a ggplot2 theme
#' object, so `theme = ggplot2::theme_minimal()` works as readily as
#' `theme = "dark"`.
#'
#' @keywords internal
.anova_theme <- function(theme = c("jeremy", "dark", "gridline", "none"),
                        base_size = 24) {

  if (inherits(theme, "theme")) return(theme)
  if (is.function(theme))       return(theme(base_size = base_size))

  switch(match.arg(theme),
    jeremy   = jermeys_theme(base_size = base_size),
    dark     = theme_black(base_size = base_size),
    gridline = gline_theme(base_size = base_size),
    none     = NULL)
}

#' Is this theme a dark one?
#'
#' Geom colours are set in the plotting code, not by the theme, so a black
#' panel would otherwise get black points drawn on it. This decides which
#' palette to hand the geoms. A user-supplied theme object is inspected for a
#' dark panel fill rather than assumed to be light.
#'
#' @keywords internal
.is_dark_theme <- function(theme) {

  if (is.character(theme)) return(identical(theme[1], "dark"))
  if (is.function(theme))  return(identical(
    tryCatch(theme()$panel.background$fill, error = function(e) NA), "black"))

  if (inherits(theme, "theme")) {
    fill <- tryCatch(theme$panel.background$fill, error = function(e) NA)
    if (is.null(fill) || length(fill) != 1 || is.na(fill)) return(FALSE)
    rgb <- tryCatch(grDevices::col2rgb(fill), error = function(e) NULL)
    if (is.null(rgb)) return(FALSE)
    return(mean(rgb) < 128)   # dark panel -> light ink
  }
  FALSE
}

#' Geom colours matched to a light or dark panel.
#' @keywords internal
.anova_palette <- function(dark = FALSE) {
  if (dark) {
    list(fill = "grey25", outline = "grey50", points = "grey75",
         ink = "white", hist_fill = "steelblue4", box_fill = "grey30")
  } else {
    list(fill = "grey92", outline = "grey70", points = "grey40",
         ink = "black", hist_fill = "lightblue", box_fill = "grey85")
  }
}

# ---- the reporting figure --------------------------------------------------

#' Integer axis breaks for a short discrete scale.
#'
#' A 1-7 Likert item labelled 2, 4, 6 is harder to read than one labelled
#' every point, because the reader has to infer where 3 and 5 are on a scale
#' that only ever took whole values. When the scale is short enough to label
#' every point, do.
#'
#' An explicit `y_range` wins. Failing that, an outcome that is entirely whole
#' numbers over a short span is treated as a discrete scale too, so forgetting
#' to declare it still gives sensible breaks.
#'
#' @return A numeric vector of breaks, or NULL to leave ggplot2's choice alone.
#' @keywords internal
.y_breaks <- function(y, y_range = NULL, max_points = 10) {

  whole <- function(x) all(abs(x - round(x)) < .Machine$double.eps^0.5,
                           na.rm = TRUE)

  if (!is.null(y_range)) {
    r <- sort(y_range)
    if (whole(r) && diff(r) <= max_points) return(seq(r[1], r[2], by = 1))
    return(NULL)
  }

  if (whole(y)) {
    r <- range(y, na.rm = TRUE)
    if (diff(r) <= max_points) return(seq(r[1], r[2], by = 1))
  }
  NULL
}

#' Size the mean point so it cannot swallow its own confidence interval.
#'
#' At large n the interval becomes very short, and a fixed-size point sits on
#' top of it and hides it. This scales the point by how much of the panel the
#' widest interval actually occupies, so the driver is the CI-to-axis ratio
#' rather than n directly. That also covers small n on a wide declared scale,
#' where the same problem appears for a different reason.
#'
#' @keywords internal
.mean_point_size <- function(ci_low, ci_high, ylim, base = 4) {

  axis_span <- diff(range(ylim))
  ci_span   <- suppressWarnings(max(ci_high - ci_low, na.rm = TRUE))
  if (!is.finite(axis_span) || axis_span <= 0 || !is.finite(ci_span)) {
    return(list(size = base, reduced = FALSE))
  }

  # a point of size s is roughly s mm across; a typical panel is ~100mm tall,
  # so the interval is about (ci_span / axis_span) * 100 mm
  frac <- ci_span / axis_span
  size <- max(1.5, min(base, frac * 100))
  list(size = size, reduced = size < base - 1e-8, frac = frac)
}
#' Choose axis limits, and explain the choice.
#'
#' Limits are computed over the data AND the confidence intervals, because a CI
#' can extend past the most extreme observation and would otherwise be clipped.
#'
#' @keywords internal
.y_limits <- function(y, ci_low, ci_high, y_range = NULL, pad = 0.10) {

  data_rng <- range(y, na.rm = TRUE)
  full_rng <- range(c(y, ci_low, ci_high), na.rm = TRUE)

  if (!is.null(y_range)) {
    if (length(y_range) != 2L || !is.numeric(y_range) || any(is.na(y_range))) {
      stop("`y_range` must be two numbers, e.g. c(1, 7).", call. = FALSE)
    }
    lim <- sort(y_range)
    if (lim[1] > full_rng[1] || lim[2] < full_rng[2]) {
      warning("`y_range` does not cover the data; the plot will be zoomed in ",
              "and some observations will fall outside the panel.",
              call. = FALSE)
    }
    return(list(lim = lim, exact = TRUE, data = data_rng, full = full_rng,
                pad = pad))
  }

  span <- diff(full_rng)
  if (span == 0) span <- max(abs(full_rng[1]), 1) * 0.1   # all values identical
  list(lim = c(full_rng[1] - pad * span, full_rng[2] + pad * span),
       exact = FALSE, data = data_rng, full = full_rng, pad = pad)
}

#' How to draw individual observations at this sample size.
#'
#' Every point at n = 2000 is a solid blob that shows less than the violin
#' behind it, so alpha and size shrink as n grows and the points come off
#' entirely past the threshold.
#'
#' @keywords internal
.point_style <- function(n_max, points = "auto") {

  if (is.logical(points)) points <- if (isTRUE(points)) "all" else "none"
  points <- match.arg(points, c("auto", "all", "none"))

  if (points == "none") {
    return(list(draw = FALSE, alpha = NA, size = NA, auto = FALSE,
                why = "points suppressed (points = \"none\")"))
  }

  style <- if (n_max <= 50) {
    list(alpha = 0.45, size = 2.0)
  } else if (n_max <= 200) {
    list(alpha = 0.30, size = 1.4)
  } else if (n_max <= 500) {
    list(alpha = 0.18, size = 0.9)
  } else {
    list(alpha = 0.10, size = 0.5)
  }

  if (points == "all") {
    return(list(draw = TRUE, alpha = style$alpha, size = style$size,
                auto = FALSE, why = NA_character_))
  }

  # points == "auto"
  if (n_max > 500) {
    return(list(
      draw = FALSE, alpha = NA, size = NA, auto = TRUE,
      why = sprintf(paste("%d observations in the largest group is too many to",
                          "plot individually, so the points were dropped and the",
                          "violin carries the distribution. Force them with",
                          "points = \"all\"."), n_max)))
  }
  list(draw = TRUE, alpha = style$alpha, size = style$size, auto = TRUE,
       why = if (n_max > 50)
         sprintf("%d per group, so the points were thinned (alpha %.2f, size %.1f).",
                 n_max, style$alpha, style$size) else NA_character_)
}

#' Violin, individual points, group mean, and confidence interval
#'
#' The figure that belongs next to a one-way ANOVA: the shape of each
#' distribution, the observations, the mean, and the uncertainty around it.
#' The caption states what the error bars are, which is not optional.
#'
#' @section The y axis:
#' By default the axis spans the data and the confidence intervals plus 10%
#' headroom each side, rather than ggplot2's tighter default. A truncated axis
#' exaggerates differences between means, so the wider default is the more
#' honest starting point.
#'
#' When the outcome has a real measurement scale, say a 1-7 Likert item or a
#' percentage, pass it: `y_range = c(1, 7)`. It is then used exactly, with no
#' padding, so the figure shows where the groups sit on the scale people
#' actually responded on.
#'
#' Limits are applied with `coord_cartesian()`, which zooms. The alternative,
#' `scale_y_continuous(limits = )`, silently **drops** observations outside the
#' range, which would change the violin and the confidence intervals rather
#' than just the view of them.
#'
#' Unless `quiet = TRUE`, the choice is reported along with the line of code to
#' paste if you want different limits.
#'
#' @param model A fitted `aov()` or `lm()`.
#' @param conf Confidence level for the bars. Default `0.95`.
#' @param pooled Model-based standard error. Default `TRUE`. See [anova_means()].
#' @param points `"auto"` (default) draws the observations, thinning them as n
#'   grows and dropping them past 500 per group; `"all"` always draws them;
#'   `"none"` never does. `TRUE`/`FALSE` are accepted as `"all"`/`"none"`.
#' @param y_range Two numbers giving the measurement scale, e.g. `c(1, 7)`,
#'   used exactly. `NULL` (default) uses the data range plus 10%.
#' @param y_breaks Axis breaks. `NULL` (default) labels every point of a
#'   whole-numbered scale spanning 10 or less, e.g. every value of a 1-7
#'   Likert item, and otherwise leaves ggplot2's choice alone.
#' @param title,xlab,ylab Passed to `labs()`; defaults come from the model.
#' @param seed Seed for the jitter, so the figure is reproducible.
#' @param theme One of `"jeremy"` (default), `"dark"`, `"gridline"`, or
#'   `"none"`; or any ggplot2 theme object or theme-returning function.
#'   Geom colours follow the theme, so `"dark"` gets light points on a black
#'   panel rather than black ones that vanish.
#' @param base_size Base font size passed to the theme. Default `24`, the
#'   themes' own default, chosen so text stays legible in a saved PNG rather
#'   than only in the RStudio pane.
#' @param quiet Suppress the note about the axis and the points.
#'
#' @return A ggplot. The axis note is also attached as the `anova_note`
#'   attribute, so it is available even when `quiet = TRUE`.
#'
#' @examples
#' \dontrun{
#' fit <- aov(weight ~ group, data = PlantGrowth)
#' anova_plot(fit, title = "Plant growth by condition")
#' anova_plot(fit, y_range = c(0, 8))     # a known measurement scale
#' anova_plot(fit, quiet = TRUE)
#' }
#' @export
anova_plot <- function(model, conf = 0.95, pooled = TRUE,
                       points = c("auto", "all", "none"),
                       y_range = NULL, y_breaks = NULL,
                       title = NULL, xlab = NULL, ylab = NULL, seed = 1,
                       theme = c("jeremy", "dark", "gridline", "none"),
                       base_size = 24, quiet = FALSE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("anova_plot() needs ggplot2.", call. = FALSE)
  }
  if (!is.logical(points)) points <- match.arg(points)

  pal <- .anova_palette(.is_dark_theme(theme))
  p   <- .model_parts(model)
  d   <- data.frame(group = p$g, y = p$y)
  m   <- anova_means(model, conf = conf, pooled = pooled)
  names(m)[1] <- "group"
  m$group <- factor(m$group, levels = levels(p$g))

  yl  <- .y_limits(d$y, m$ci_low, m$ci_high, y_range)
  pts <- .point_style(max(m$n), points)
  brk <- if (is.null(y_breaks)) .y_breaks(d$y, y_range) else y_breaks
  mps <- .mean_point_size(m$ci_low, m$ci_high, yl$lim)

  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$group, y = .data$y)) +
    ggplot2::geom_violin(fill = pal$fill, color = pal$outline, width = 0.8)

  if (pts$draw) {
    set.seed(seed)
    g <- g + ggplot2::geom_jitter(width = 0.08, alpha = pts$alpha,
                                  size = pts$size, color = pal$points)
  }

  g <- g +
    ggplot2::geom_errorbar(
      data = m,
      ggplot2::aes(x = .data$group, ymin = .data$ci_low, ymax = .data$ci_high),
      width = 0.08, linewidth = 0.9, colour = pal$ink, inherit.aes = FALSE) +
    ggplot2::geom_point(
      data = m, ggplot2::aes(x = .data$group, y = .data$mean),
      size = mps$size, colour = pal$ink, inherit.aes = FALSE) +
    ggplot2::coord_cartesian(ylim = yl$lim) +
    ggplot2::labs(
      x = if (is.null(xlab)) p$g_name else xlab,
      y = if (is.null(ylab)) p$y_name else ylab,
      title = title,
      # wrapped: at base_size 24 an unwrapped caption runs off the panel
      caption = paste(strwrap(sprintf(
        "Large point = group mean, bars = %.0f%% CI (%s).%s",
        conf * 100, attr(m, "se_type"),
        if (pts$draw) " Small points are individual observations." else ""),
        width = 70), collapse = "\n")
    ) +
    .anova_theme(theme, base_size) +
    # the themes do not style the caption, so on a dark panel it would
    # inherit grey30 and disappear
    ggplot2::theme(plot.caption = ggplot2::element_text(
      colour = pal$ink, size = base_size * 0.6, hjust = 0))

  # breaks only. scale_y_continuous(limits = ) would drop observations;
  # the zooming is coord_cartesian()'s job, set above.
  if (!is.null(brk)) {
    g <- g + ggplot2::scale_y_continuous(breaks = brk)
  }

  note <- .axis_note(yl, pts, brk, mps)
  attr(g, "anova_note") <- note
  if (!quiet) message(note)
  g
}

#' Build the note explaining the axis and point decisions.
#' @keywords internal
.axis_note <- function(yl, pts, brk = NULL, mps = NULL) {

  fmt <- function(x) format(x, digits = 4, trim = TRUE)

  axis_line <- if (yl$exact) {
    sprintf(paste("Y axis: data ranged %s to %s; axis set to %s to %s exactly,",
                  "as given by y_range."),
            fmt(yl$data[1]), fmt(yl$data[2]), fmt(yl$lim[1]), fmt(yl$lim[2]))
  } else {
    sprintf(paste("Y axis: data ranged %s to %s; axis set to %s to %s, which is",
                  "the data and CIs plus %d%% each side. For a real measurement",
                  "scale (a 1-7 Likert item, a percentage) pass it exactly, e.g.",
                  "y_range = c(1, 7)."),
            fmt(yl$data[1]), fmt(yl$data[2]), fmt(yl$lim[1]), fmt(yl$lim[2]),
            round(yl$pad * 100))
  }

  parts <- c(paste(strwrap(axis_line, width = 76), collapse = "\n"))

  if (!is.na(pts$why)) {
    parts <- c(parts, paste(strwrap(paste("Points:", pts$why), width = 76),
                            collapse = "\n"))
  }

  if (!is.null(brk) && length(brk) > 1) {
    parts <- c(parts, paste(strwrap(sprintf(paste(
      "Breaks: every point from %s to %s, since the scale is whole-numbered",
      "and short enough to label in full."),
      fmt(min(brk)), fmt(max(brk))), width = 76), collapse = "\n"))
  }

  if (!is.null(mps) && isTRUE(mps$reduced)) {
    parts <- c(parts, paste(strwrap(sprintf(paste(
      "Mean point: shrunk to %.1f because the widest CI spans only %.1f%% of",
      "the axis, and a full-size point would cover it."),
      mps$size, mps$frac * 100), width = 76), collapse = "\n"))
  }

  parts <- c(parts,
             "To adjust the axis by hand, add this to the plot:",
             sprintf("  + ggplot2::coord_cartesian(ylim = c(%s, %s))",
                     fmt(yl$lim[1]), fmt(yl$lim[2])))

  paste(parts, collapse = "\n")
}

# ---- the test, effect sizes, and the sentence ------------------------------

#' F test with effect sizes and a ready-to-paste APA sentence
#'
#' Reports the omnibus F, both effect size measures, and a formatted sentence.
#'
#' `R^2` (MDK Equation 94) is the proportion of variance accounted for in this
#' sample. Most of psychology calls the same quantity eta squared in the ANOVA
#' context; they are identical in a one-way design. It is biased upward as an
#' estimate of the population value.
#'
#' `omega^2` (MDK Equation 96) corrects that bias and is the one to report. It
#' can come out negative, in which case it is reported as zero.
#'
#' @param model A fitted `aov()` or `lm()`.
#'
#' @return An object of class `anova_report`.
#'
#' @examples
#' anova_report(aov(weight ~ group, data = PlantGrowth))
#'
#' @seealso [anova_ref()] with `"r_squared"` or `"omega_squared"`.
#' @export
anova_report <- function(model) {

  tab <- stats::anova(model)

  # One predictor plus Residuals is two rows. Anything more is a
  # repeated-measures or factorial model, where SS_total is not simply
  # SS_effect + SS_residual and this omega squared would be wrong. Refuse
  # rather than return a plausible-looking bad number.
  if (nrow(tab) > 2L) {
    stop("anova_report() handles one-way between-subjects models only.\n",
         "  This model has ", nrow(tab) - 1L, " terms (",
         paste(rownames(tab)[-nrow(tab)], collapse = ", "), ").\n",
         "  For repeated-measures and factorial designs the right effect size\n",
         "  is generalized eta squared, which is not implemented yet:\n",
         "  see anova_ref(\"generalized_eta_squared\").", call. = FALSE)
  }

  eff <- rownames(tab)[1]

  SS_eff <- tab[eff, "Sum Sq"]
  SS_err <- tab["Residuals", "Sum Sq"]
  df_eff <- tab[eff, "Df"]
  df_err <- tab["Residuals", "Df"]
  MS_W   <- tab["Residuals", "Mean Sq"]
  Fval   <- tab[eff, "F value"]
  pval   <- tab[eff, "Pr(>F)"]

  SS_tot <- SS_eff + SS_err
  r2     <- SS_eff / SS_tot
  omega2 <- max((SS_eff - df_eff * MS_W) / (SS_tot + MS_W), 0)

  structure(
    list(formula = stats::formula(model), effect = eff,
         F = Fval, df1 = df_eff, df2 = df_err, p = pval, MS_W = MS_W,
         r2 = r2, omega2 = omega2,
         apa = sprintf("F(%d, %d) = %.2f, p %s, omega-squared = %s",
                       df_eff, df_err, Fval, .p_phrase(pval),
                       sub("^0", "", sprintf("%.2f", omega2))),
         table = tab),
    class = "anova_report")
}

#' Print an ANOVA report
#'
#' @param x An `anova_report` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.anova_report <- function(x, ...) {
  cat("\nOne-way ANOVA:", deparse(x$formula), "\n\n")
  cat(sprintf("  F(%d, %d) = %.2f, p %s\n", x$df1, x$df2, x$F, .p_phrase(x$p)))
  cat(sprintf("  R^2     = %.3f   (= eta^2; variance accounted for in this sample)\n", x$r2))
  cat(sprintf("  omega^2 = %.3f   (bias-corrected, report this one)\n", x$omega2))
  cat("\n  APA: ", x$apa, "\n", sep = "")
  cat("\n  anova_ref(\"omega_squared\") for why omega squared rather than R^2.\n\n")
  invisible(x)
}


# ---- everything at once ----------------------------------------------------

#' Assumptions, test, means, and figure in one call
#'
#' @param object A fitted `aov()`/`lm()`, or a formula.
#' @param data A data frame. Required when `object` is a formula.
#' @param id Name of the subject identifier column, for within-subjects
#'   designs. Passed through to [anova_check()].
#' @param plot Draw the figure. Default `TRUE`.
#' @param ... Passed to [anova_plot()].
#'
#' @return Invisibly, a list with `check`, `means`, `report`, and `figure`.
#'
#' @examples
#' \dontrun{
#' anova_workup(weight ~ group, data = PlantGrowth)
#' }
#' @export
anova_workup <- function(object, data = NULL, id = NULL, plot = TRUE, ...) {

  chk <- anova_check(object, data = data, id = id)
  print(chk)

  if (identical(chk$design, "oneway_ws")) {
    cat(strrep("-", 62), "\n\n")
    cat("Repeated-measures F test (uncorrected and corrected):\n\n")
    print(stats::anova(stats::lm(chk$wide ~ 1), X = ~1, test = "Spherical"))
    cat("\n  Effect size is not reported for within-subjects designs yet;\n")
    cat("  generalized eta squared is the right measure. See\n")
    cat("  anova_ref(\"generalized_eta_squared\").\n\n")
    return(invisible(list(check = chk, means = NULL, report = NULL, figure = NULL)))
  }

  if (!is.null(chk$model)) {
    cat(strrep("-", 62), "\n")
    rep <- anova_report(chk$model)
    print(rep)
    mns <- anova_means(chk$model)
    cat("Group means (", attr(mns, "se_type"), "):\n\n", sep = "")
    print(mns, row.names = FALSE, digits = 4)
    cat("\n")

    fig <- NULL
    if (plot && requireNamespace("ggplot2", quietly = TRUE)) {
      fig <- anova_plot(chk$model, ...)
      print(fig)
    }
    return(invisible(list(check = chk, means = mns, report = rep, figure = fig)))
  }

  invisible(list(check = chk, means = NULL, report = NULL, figure = NULL))
}


# ---- diagnostic plots ------------------------------------------------------

#' Diagnostic plots for an assumption check
#'
#' Q-Q plot of the residuals, a histogram of the residuals, and a boxplot of
#' the outcome by condition. Returned as a named list rather than drawn, so you
#' can print the one you want or arrange them however you like.
#'
#' The Q-Q plot is the one to read when a normality test is significant. It
#' separates the two cases that matter and that a p value cannot distinguish:
#' a few extreme observations, versus genuine skew or heavy tails through the
#' whole distribution.
#'
#' @param x An `anova_check` object, or a fitted `aov()`/`lm()`.
#' @param bins Bins for the residual histogram.
#' @param theme One of `"jeremy"` (default), `"dark"`, `"gridline"`, or
#'   `"none"`; or any ggplot2 theme object or theme-returning function.
#' @param base_size Base font size passed to the theme. Default `24`.
#'
#' @return A named list of ggplots: `qq`, `residuals`, `boxplot`.
#'
#' @examples
#' \dontrun{
#' chk <- anova_check(weight ~ group, data = PlantGrowth)
#' p <- anova_check_plots(chk)
#' p$qq
#' }
#'
#' @seealso [anova_check()], [anova_ref()] with `"normality"`.
#' @export
anova_check_plots <- function(x, bins = 12,
                              theme = c("jeremy", "dark", "gridline", "none"),
                              base_size = 24) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("anova_check_plots() needs ggplot2.", call. = FALSE)
  }
  pal <- .anova_palette(.is_dark_theme(theme))
  thm <- .anova_theme(theme, base_size)

  if (inherits(x, "anova_check")) {
    model  <- x$model
    y_name <- x$y_name
    g_name <- x$g_name
  } else if (inherits(x, c("aov", "lm"))) {
    model  <- x
    pp     <- .model_parts(model)
    y_name <- pp$y_name
    g_name <- pp$g_name
  } else {
    stop("`x` must be an anova_check object or a fitted aov()/lm().", call. = FALSE)
  }

  mf  <- stats::model.frame(model)
  res <- as.vector(stats::residuals(model))
  d   <- data.frame(resid = res, group = factor(mf[[2]]), y = mf[[1]])

  qq <- ggplot2::ggplot(d, ggplot2::aes(sample = .data$resid)) +
    ggplot2::stat_qq(colour = pal$ink) +
    ggplot2::stat_qq_line(colour = pal$outline) +
    ggplot2::labs(x = "Theoretical quantile", y = "Sample quantile",
                  title = "Q-Q plot of residuals") + thm

  hist <- ggplot2::ggplot(d, ggplot2::aes(x = .data$resid)) +
    ggplot2::geom_histogram(bins = bins, fill = pal$hist_fill, color = pal$ink) +
    ggplot2::labs(x = "Residual", y = "Count",
                  title = "Residuals from the model") + thm

  box <- ggplot2::ggplot(d, ggplot2::aes(x = .data$group, y = .data$y)) +
    ggplot2::geom_boxplot(fill = pal$box_fill, colour = pal$ink) +
    ggplot2::labs(x = g_name, y = y_name,
                  title = "Spread within each condition") + thm

  list(qq = qq, residuals = hist, boxplot = box)
}
