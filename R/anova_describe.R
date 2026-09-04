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
                        base_size = 14) {

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

#' Violin, individual points, group mean, and confidence interval
#'
#' The figure that belongs next to a one-way ANOVA: the shape of each
#' distribution, every observation, the mean, and the uncertainty around it.
#' The caption states what the error bars are, which is not optional.
#'
#' @param model A fitted `aov()` or `lm()`.
#' @param conf Confidence level for the bars. Default `0.95`.
#' @param pooled Model-based standard error. Default `TRUE`. See [anova_means()].
#' @param points Draw individual observations. Turn off for large n.
#' @param title,xlab,ylab Passed to `labs()`; defaults come from the model.
#' @param seed Seed for the jitter, so the figure is reproducible.
#' @param theme One of `"jeremy"` (default), `"dark"`, `"gridline"`, or
#'   `"none"`; or any ggplot2 theme object or theme-returning function.
#'   Geom colours follow the theme, so `"dark"` gets light points on a black
#'   panel rather than black ones that vanish.
#' @param base_size Base font size passed to the theme. Default `14`; the
#'   themes themselves default to 24, which suits slides more than figures.
#'
#' @return A ggplot.
#'
#' @examples
#' \dontrun{
#' fit <- aov(weight ~ group, data = PlantGrowth)
#' anova_plot(fit, title = "Plant growth by condition")
#' }
#' @export
anova_plot <- function(model, conf = 0.95, pooled = TRUE, points = TRUE,
                       title = NULL, xlab = NULL, ylab = NULL, seed = 1,
                       theme = c("jeremy", "dark", "gridline", "none"),
                       base_size = 14) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("anova_plot() needs ggplot2.", call. = FALSE)
  }
  pal <- .anova_palette(.is_dark_theme(theme))
  p <- .model_parts(model)
  d <- data.frame(group = p$g, y = p$y)
  m <- anova_means(model, conf = conf, pooled = pooled)
  names(m)[1] <- "group"
  m$group <- factor(m$group, levels = levels(p$g))

  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$group, y = .data$y)) +
    ggplot2::geom_violin(fill = pal$fill, color = pal$outline, width = 0.8)

  if (points) {
    set.seed(seed)
    g <- g + ggplot2::geom_jitter(width = 0.08, alpha = 0.45, size = 2,
                                  color = pal$points)
  }

  g +
    ggplot2::geom_errorbar(
      data = m,
      ggplot2::aes(x = .data$group, ymin = .data$ci_low, ymax = .data$ci_high),
      width = 0.08, linewidth = 0.9, colour = pal$ink, inherit.aes = FALSE) +
    ggplot2::geom_point(
      data = m, ggplot2::aes(x = .data$group, y = .data$mean),
      size = 4, colour = pal$ink, inherit.aes = FALSE) +
    ggplot2::labs(
      x = if (is.null(xlab)) p$g_name else xlab,
      y = if (is.null(ylab)) p$y_name else ylab,
      title = title,
      # wrapped: at larger base_size an unwrapped caption runs off the panel
      caption = paste(strwrap(sprintf(
        "Large point = group mean, bars = %.0f%% CI (%s).%s",
        conf * 100, attr(m, "se_type"),
        if (points) " Small points are individual observations." else ""),
        width = 70), collapse = "
")
    ) +
    .anova_theme(theme, base_size) +
    # the themes do not style the caption, so on a dark panel it would
    # inherit grey30 and disappear
    ggplot2::theme(plot.caption = ggplot2::element_text(
      colour = pal$ink, size = base_size * 0.6, hjust = 0))
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
#' @param base_size Base font size passed to the theme. Default `14`.
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
                              base_size = 14) {

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
