# ============================================================
# anova_multiverse.R
#
# Layer 6: mini-multiverse / specification-curve analysis.
#
# Field and Wilcox (2017) recommend that every frequentist analysis be
# accompanied by a sensitivity analysis: fit the conventional model AND
# a robust counterpart, report either where they agree, report the
# robust one where they do not. Their stronger claim is that this
# comparison is the only known way to judge whether a conventional
# method gave a reasonable answer.
#
# anova_multi() automates that, and widens it: instead of one robust
# counterpart, cross the small analysis decisions you could defensibly
# have made and look at whether the conclusion survives all of them.
#
# WHAT GOES ON THE PLOT
#   The omnibus F has no single signed effect, so the forest plot is
#   anchored on a FOCAL CONTRAST (two groups), standardized so that
#   transformed and untransformed specifications land on a common axis.
#   The omnibus test lives in the results table instead.
#
# DEPENDENCIES
#   WRS2 is in Suggests, not Imports. Trimmed-means specifications run
#   if it is installed and are skipped with a reason if it is not.
# ============================================================


# ---- small internals -------------------------------------------------------

#' Winsorized variance, base R.
#' @keywords internal
.winvar <- function(x, tr = 0.2) {
  y <- sort(x)
  n <- length(y)
  g <- floor(tr * n)
  if (g > 0) {
    y[seq_len(g + 1)]     <- y[g + 1]
    y[(n - g):n]          <- y[n - g]
  }
  stats::var(y)
}

#' Pooled SD of two samples.
#' @keywords internal
.pooled_sd <- function(a, b) {
  na <- length(a); nb <- length(b)
  sqrt(((na - 1) * stats::var(a) + (nb - 1) * stats::var(b)) / (na + nb - 2))
}

#' Pooled winsorized SD of two samples.
#' @keywords internal
.pooled_winsd <- function(a, b, tr = 0.2) {
  na <- length(a); nb <- length(b)
  sqrt(((na - 1) * .winvar(a, tr) + (nb - 1) * .winvar(b, tr)) / (na + nb - 2))
}

#' Apply one of the supported transformations, or report why it cannot be.
#' @keywords internal
.apply_transform <- function(y, how) {
  switch(how,
    none = list(ok = TRUE, y = y),
    log  = if (any(y <= 0, na.rm = TRUE))
             list(ok = FALSE, why = "log needs strictly positive values")
           else list(ok = TRUE, y = log(y)),
    sqrt = if (any(y < 0, na.rm = TRUE))
             list(ok = FALSE, why = "sqrt needs non-negative values")
           else list(ok = TRUE, y = sqrt(y)),
    stop("Unknown transformation: ", how, call. = FALSE)
  )
}

#' Is WRS2 available?
#' @keywords internal
.have_wrs2 <- function() requireNamespace("WRS2", quietly = TRUE)


# ---- the decision grid -----------------------------------------------------

#' Default set of analysis decisions crossed by anova_multi().
#'
#' Outlier trimming is deliberately NOT on by default. Field and Wilcox are
#' critical of SD-based trims followed by OLS, so it is available on request
#' and labelled in the output, rather than presented as a routine option.
#'
#' @keywords internal
.anova_default_decisions <- list(
  transform = c("none", "log", "sqrt"),
  variance  = c("pooled", "welch"),
  estimator = c("ols", "trim20")
)

#' Build the specification grid, collapsing redundant cells.
#'
#' A trimmed-means test is already heteroscedastic-robust, so crossing it
#' with the pooled/Welch decision would produce duplicate rows. Those cells
#' are collapsed to a single `variance = "robust"` row.
#'
#' @keywords internal
.build_specs <- function(decisions) {

  grid <- expand.grid(decisions, stringsAsFactors = FALSE,
                      KEEP.OUT.ATTRS = FALSE)

  if (!is.null(grid$estimator)) {
    is_trim <- grid$estimator != "ols"
    if (any(is_trim) && !is.null(grid$variance)) {
      grid$variance[is_trim] <- "robust"
      grid <- unique(grid)
    }
  }
  rownames(grid) <- NULL
  grid
}


# ---- fitting one specification ---------------------------------------------

#' Fit a single specification and return one standard result row.
#' @keywords internal
.fit_spec <- function(spec, y, g, contrast, conf, tr) {

  blank <- data.frame(
    F = NA_real_, df1 = NA_real_, df2 = NA_real_, p_omnibus = NA_real_,
    effect = NA_real_, effect_type = NA_character_,
    est = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
    p_contrast = NA_real_, n_used = NA_integer_,
    ok = FALSE, why = NA_character_, stringsAsFactors = FALSE
  )

  tf <- .apply_transform(y, spec$transform)
  if (!tf$ok) { blank$why <- tf$why; return(blank) }
  yy <- tf$y

  keep <- stats::complete.cases(yy, g)
  yy <- yy[keep]; gg <- droplevels(factor(g[keep]))

  a  <- yy[gg == contrast[1]]
  b  <- yy[gg == contrast[2]]
  if (length(a) < 3 || length(b) < 3) {
    blank$why <- "fewer than 3 observations in a contrast group"
    return(blank)
  }

  out <- blank
  out$n_used <- length(yy)

  if (spec$estimator == "ols") {

    # omnibus
    if (spec$variance == "welch") {
      ow <- stats::oneway.test(yy ~ gg, var.equal = FALSE)
      out$F <- unname(ow$statistic); out$df1 <- unname(ow$parameter[1])
      out$df2 <- unname(ow$parameter[2]); out$p_omnibus <- ow$p.value
    } else {
      tab <- stats::anova(stats::lm(yy ~ gg))
      out$F <- tab[1, "F value"]; out$df1 <- tab[1, "Df"]
      out$df2 <- tab[2, "Df"];    out$p_omnibus <- tab[1, "Pr(>F)"]
    }

    # effect size: omega squared, MDK eq. 96
    tab  <- stats::anova(stats::lm(yy ~ gg))
    SS_e <- tab[1, "Sum Sq"]; SS_r <- tab[2, "Sum Sq"]
    MS_W <- tab[2, "Mean Sq"]; df_e <- tab[1, "Df"]
    out$effect <- max((SS_e - df_e * MS_W) / (SS_e + SS_r + MS_W), 0)
    out$effect_type <- "omega2"

    # focal contrast, standardized by the pooled SD on this scale
    tt <- stats::t.test(a, b, var.equal = (spec$variance == "pooled"),
                        conf.level = conf)
    s  <- .pooled_sd(a, b)
    out$est        <- unname(tt$estimate[1] - tt$estimate[2]) / s
    out$ci_low     <- tt$conf.int[1] / s
    out$ci_high    <- tt$conf.int[2] / s
    out$p_contrast <- tt$p.value
    out$ok <- TRUE

  } else {

    if (!.have_wrs2()) {
      out$why <- "WRS2 not installed; install.packages(\"WRS2\") to run robust specs"
      return(out)
    }

    d1 <- data.frame(yy = yy, gg = gg)
    t1 <- try(WRS2::t1way(yy ~ gg, data = d1, tr = tr), silent = TRUE)
    if (inherits(t1, "try-error")) { out$why <- "WRS2::t1way() failed"; return(out) }

    out$F <- t1$test; out$df1 <- t1$df1; out$df2 <- t1$df2
    out$p_omnibus <- t1$p.value
    out$effect <- t1$effsize
    out$effect_type <- "xi"   # explanatory measure of effect size, not omega2

    d2 <- data.frame(yy = c(a, b),
                     gg = factor(rep(contrast, c(length(a), length(b))),
                                 levels = contrast))
    yn <- try(WRS2::yuen(yy ~ gg, data = d2, tr = tr), silent = TRUE)
    if (inherits(yn, "try-error")) { out$why <- "WRS2::yuen() failed"; return(out) }

    # Robust standardized difference: trimmed mean difference over the pooled
    # winsorized SD, rescaled by 0.642 so it is on the same footing as Cohen's
    # d under normality (Algina, Keselman & Penfield, 2005).
    s <- .pooled_winsd(a, b, tr)
    out$est        <- 0.642 * unname(yn$diff) / s
    out$ci_low     <- 0.642 * yn$conf.int[1] / s
    out$ci_high    <- 0.642 * yn$conf.int[2] / s
    out$p_contrast <- yn$p.value
    out$ok <- TRUE
  }

  out
}


# ---- Layer 1: the orchestrator ---------------------------------------------

#' Mini-multiverse analysis for a one-way design
#'
#' Crosses the small analysis decisions you could defensibly have made,
#' refits under each, and reports whether the conclusion survives. This
#' operationalises the sensitivity analysis Field and Wilcox (2017) argue
#' should accompany every frequentist analysis.
#'
#' The forest plot is anchored on a **focal contrast** between two groups,
#' standardized so that transformed and untransformed specifications share an
#' axis. The omnibus test appears in the results table.
#'
#' @param formula A two-sided formula, `outcome ~ group`.
#' @param data A data frame.
#' @param contrast Length-2 character vector naming the two groups to contrast,
#'   e.g. `c("high", "vehicle")`. Defaults to the last and first factor levels.
#' @param decisions Named list of decisions to cross. Defaults to
#'   `transform = c("none","log","sqrt")`, `variance = c("pooled","welch")`,
#'   `estimator = c("ols","trim20")`.
#' @param conf Confidence level for the contrast intervals.
#' @param trim Trim proportion for the robust specifications. Default `.2`.
#' @param alpha Threshold used to judge whether specifications disagree.
#'
#' @return An object of class `anova_multiverse`, with `results` (one row per
#'   specification), `baseline`, and `n_disagree`.
#'
#' @examples
#' \dontrun{
#' mv <- anova_multi(weight ~ group, data = PlantGrowth,
#'                   contrast = c("trt2", "ctrl"))
#' mv
#' anova_multi_plot(mv)
#' }
#'
#' @seealso [anova_ref()], with `"robust_sensitivity"`, for the argument this
#'   implements; [anova_multi_plot()] for the forest plot.
#' @importFrom stats anova lm oneway.test t.test complete.cases var
#' @export
anova_multi <- function(formula, data, contrast = NULL, decisions = NULL,
                        conf = 0.95, trim = 0.2, alpha = 0.05) {

  mf <- stats::model.frame(formula, data = data)
  if (ncol(mf) != 2L) stop("anova_multi() handles one-way designs.", call. = FALSE)
  y <- mf[[1]]
  g <- factor(mf[[2]])

  if (is.null(contrast)) contrast <- c(levels(g)[nlevels(g)], levels(g)[1])
  if (length(contrast) != 2L || !all(contrast %in% levels(g))) {
    stop("`contrast` must name two levels of ", names(mf)[2], ": ",
         paste(levels(g), collapse = ", "), call. = FALSE)
  }

  if (is.null(decisions)) decisions <- .anova_default_decisions
  specs <- .build_specs(decisions)

  rows <- lapply(seq_len(nrow(specs)), function(i) {
    .fit_spec(as.list(specs[i, , drop = FALSE]), y, g, contrast, conf, trim)
  })
  res <- cbind(specs, do.call(rbind, rows))

  res$label <- apply(specs, 1, function(r) paste(r, collapse = " / "))

  res$sig      <- res$p_contrast < alpha
  res$sig_omni <- res$p_omnibus  < alpha
  res$ci_width <- res$ci_high - res$ci_low

  # The baseline specification: nothing transformed, pooled variance, OLS.
  # Snapshot it only AFTER the derived columns exist, or base$sig is NULL and
  # the comparison below silently collapses to logical(0).
  is_base <- res$transform == "none" & res$variance == "pooled" &
             res$estimator == "ols"
  base_i <- if (any(is_base)) which(is_base)[1] else 1L
  base   <- res[base_i, ]

  res$disagrees <- res$ok & !is.na(res$sig) & !is.na(base$sig) &
                   (res$sig != base$sig)

  # precision cost: CI width relative to the baseline specification
  res$width_vs_base <- res$ci_width / base$ci_width

  structure(
    list(formula = formula, y_name = names(mf)[1], g_name = names(mf)[2],
         contrast = contrast, decisions = decisions, results = res,
         baseline = base, conf = conf, trim = trim, alpha = alpha,
         n_disagree = sum(res$disagrees, na.rm = TRUE),
         n_skipped = sum(!res$ok)),
    class = "anova_multiverse"
  )
}


# ---- Layer 5: presentation -------------------------------------------------

#' Print a multiverse analysis
#'
#' @param x An `anova_multiverse` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.anova_multiverse <- function(x, ...) {

  title <- sprintf("Mini-multiverse: %s ~ %s", x$y_name, x$g_name)
  cat("\n", title, "\n", strrep("=", nchar(title)), "\n\n", sep = "")
  cat(sprintf("  Focal contrast: %s vs %s, standardized\n",
              x$contrast[1], x$contrast[2]))
  cat(sprintf("  %d specifications (%d could not be fitted)\n\n",
              nrow(x$results), x$n_skipped))

  r <- x$results
  cat(sprintf("  %-26s %8s %8s %9s %8s %7s\n",
              "specification", "d", "p", "omnibus p", "effect", "CI vs base"))
  cat("  ", strrep("-", 70), "\n", sep = "")

  for (i in seq_len(nrow(r))) {
    if (!r$ok[i]) {
      cat(sprintf("  %-26s   skipped: %s\n", r$label[i], r$why[i]))
      next
    }
    flag <- if (isTRUE(r$disagrees[i])) " <-- differs" else ""
    cat(sprintf("  %-26s %8.2f %8s %9s %8.3f %6.2fx%s\n",
                r$label[i], r$est[i], .p_phrase(r$p_contrast[i]),
                .p_phrase(r$p_omnibus[i]), r$effect[i],
                r$width_vs_base[i], flag))
  }

  cat("\n")
  if (x$n_disagree == 0) {
    cat("  Every fitted specification agrees with the baseline on significance\n")
    cat("  at alpha = ", x$alpha, ". The conclusion does not hinge on these\n", sep = "")
    cat("  analysis decisions, which is what you want to be able to say.\n")
  } else {
    cat("  ", x$n_disagree, " specification(s) disagree with the baseline on\n", sep = "")
    cat("  significance. Field and Wilcox: where models deviate, report the\n")
    cat("  robust one unless there is an evidence-based case that the\n")
    cat("  assumptions were met. Do not pick the one you liked.\n")
  }

  if (any(r$effect_type == "xi", na.rm = TRUE)) {
    cat("\n  NOTE: effect is omega^2 for OLS rows and xi (explanatory measure)\n")
    cat("  for trimmed rows. They are not the same quantity; compare within\n")
    cat("  estimator, not across.\n")
  }
  if (any(!r$ok & grepl("WRS2", r$why))) {
    cat("\n  Robust specifications were skipped. install.packages(\"WRS2\")\n")
  }

  cat("\n  anova_multi_plot() for the forest plot.\n\n")
  invisible(x)
}

#' Forest plot of a multiverse analysis
#'
#' One row per specification, showing the standardized focal contrast and its
#' confidence interval. Specifications that disagree with the baseline on
#' significance are coloured and labelled in bold, so a conclusion that depends
#' on an analysis decision is visible at a glance.
#'
#' @param x An `anova_multiverse` object.
#' @param title Optional plot title.
#'
#' @return A ggplot.
#' @export
anova_multi_plot <- function(x, title = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("anova_multi_plot() needs ggplot2.", call. = FALSE)
  }
  r <- x$results[x$results$ok, , drop = FALSE]
  if (!nrow(r)) stop("No specifications were successfully fitted.", call. = FALSE)

  r$status <- ifelse(r$disagrees, "differs from baseline",
                     ifelse(r$sig, "significant", "not significant"))

  # Disagreeing rows are marked in the label as well as coloured. ggplot2 does
  # not officially support a vectorised `face` in element_text(), and warns
  # about it, so the emphasis is carried by the marker and the colour rather
  # than by bolding individual axis labels.
  r$label <- ifelse(r$disagrees, paste0("* ", r$label), r$label)
  r$label <- factor(r$label, levels = rev(r$label))

  ggplot2::ggplot(r, ggplot2::aes(x = .data$est, y = .data$label,
                                  colour = .data$status)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high),
                           orientation = "y", width = 0.25, linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(size = .data$disagrees)) +
    ggplot2::scale_size_manual(values = c(`TRUE` = 4, `FALSE` = 2.8),
                               guide = "none") +
    ggplot2::scale_colour_manual(values = c(
      "significant"           = "grey20",
      "not significant"       = "grey60",
      "differs from baseline" = "#C1272D")) +
    ggplot2::labs(
      x = sprintf("Standardized difference, %s vs %s (%.0f%% CI)",
                  x$contrast[1], x$contrast[2], x$conf * 100),
      y = NULL, colour = NULL,
      title = if (is.null(title))
        sprintf("Does the %s effect survive the analysis decisions?", x$g_name)
      else title,
      caption = paste("Each row is one defensible analysis. Dashed line = no effect.",
                      "\nRows marked * disagree with the baseline on significance.")
    ) +
    ggplot2::theme(legend.position = "top")
}
