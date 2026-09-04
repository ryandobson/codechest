# ============================================================
# anova_assumptions.R
#
# Layers 1-3: the orchestrator, the design engines, and the tests.
#
# See anova_architecture.md. The invariant that matters:
#
#   A test produces a FINDING. A finding does not know what to do
#   about itself. The recommender (bottom of this file) is the only
#   place that decides what a pattern of findings means.
#
# Nothing here prints. print.anova_check() is Layer 5.
# ============================================================


# ---- Layer 3: findings -----------------------------------------------------

#' Construct a finding.
#'
#' The standard shape every test returns, so the printer and the recommender
#' never have to special-case one.
#'
#' @param severity One of "ok", "watch", "violated", "untestable".
#' @keywords internal
.finding <- function(concept, label, severity,
                     statistic = NA_real_, df = NA_real_, p = NA_real_,
                     stat_name = NA_character_, detail = NA_character_) {
  stopifnot(severity %in% c("ok", "watch", "violated", "untestable"))
  structure(
    list(concept = concept, label = label, severity = severity,
         statistic = statistic, df = df, p = p,
         stat_name = stat_name, detail = detail),
    class = "anova_finding"
  )
}

#' Levene's test for homogeneity of variance, in base R.
#'
#' A one-way ANOVA on each observation's absolute deviation from its own
#' group's center. `center = "mean"` is the original Levene test and matches
#' `car::leveneTest(center = mean)` exactly; `center = "median"` is the
#' Brown-Forsythe variant, which is more robust to skew.
#'
#' Implemented here rather than importing `car`, which would be a new
#' dependency for six lines of arithmetic.
#'
#' @keywords internal
.levene <- function(y, g, center = c("mean", "median")) {
  center <- match.arg(center)
  f   <- if (center == "median") stats::median else mean
  ctr <- tapply(y, g, f)
  z   <- abs(y - ctr[as.character(g)])
  tab <- stats::anova(stats::lm(z ~ g))
  list(statistic = tab[1, "F value"], df1 = tab[1, "Df"],
       df2 = tab[2, "Df"], p.value = tab[1, "Pr(>F)"], center = center)
}

#' Normality of residuals, as a finding.
#' @keywords internal
.test_normality <- function(model, alpha = 0.05) {

  res <- stats::residuals(model)
  n   <- length(res)

  if (n < 3L)   return(.finding("normality", "Normality of residuals", "untestable",
                                detail = "fewer than 3 residuals"))
  if (n > 5000L) return(.finding("normality", "Normality of residuals", "untestable",
                                 detail = "Shapiro-Wilk is limited to n <= 5000; use the Q-Q plot"))

  sw <- stats::shapiro.test(res)

  sev <- if (sw$p.value >= alpha) {
    "ok"
  } else if (n > 200L) {
    # At large n Shapiro-Wilk detects departures too small to affect F.
    "watch"
  } else {
    "violated"
  }

  detail <- if (sev == "watch") {
    sprintf("significant, but n = %d is large enough that trivial departures register; read the Q-Q plot", n)
  } else if (n < 20L) {
    sprintf("n = %d, so this test has low power to detect a real departure", n)
  } else NA_character_

  .finding("normality", "Normality of residuals", sev,
           statistic = unname(sw$statistic), p = sw$p.value,
           stat_name = "W", detail = detail)
}

#' Homogeneity of variance, as a finding.
#' @keywords internal
.test_homogeneity <- function(y, g, center = "mean", alpha = 0.05) {

  lv    <- .levene(y, g, center)
  vars  <- tapply(y, g, stats::var)
  ratio <- max(vars) / min(vars)

  sev <- if (lv$p.value < alpha) {
    "violated"
  } else if (ratio > 4) {
    # Levene has poor power at small n; a large ratio is worth flagging
    # even when the test comes back non-significant.
    "watch"
  } else {
    "ok"
  }

  detail <- sprintf("largest / smallest group variance = %.2f%s", ratio,
                    if (sev == "watch")
                      "; the test is non-significant but that ratio is large"
                    else "")

  .finding("homogeneity", "Homogeneity of variance", sev,
           statistic = lv$statistic, df = c(lv$df1, lv$df2), p = lv$p.value,
           stat_name = "F", detail = detail)
}

#' Display label for a variable, falling back to its name.
#'
#' Reads the "label" attribute that `apply_variable_info()` attaches, so a
#' plot picks up "Dried plant weight (g)" rather than "weight" without being
#' told twice. Base R rather than the labelled package, matching
#' `dynamic_histogram()`.
#'
#' @keywords internal
.nice_label <- function(x, fallback) {
  lab <- attr(x, "label", exact = TRUE)
  if (is.null(lab) || !is.character(lab) || length(lab) != 1L || !nzchar(lab)) {
    return(fallback)
  }
  lab
}

#' Drop labelled/vctrs classes so ggplot2 sees a plain vector.
#'
#' haven_labelled inherits from vctrs_vctr, which ggplot2 will not scale.
#' The label attribute is read before this is called.
#'
#' @keywords internal
.strip_labelled <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "vctrs_vctr")) {
    return(as.vector(unclass(x)))
  }
  x
}

#' Pull the response and the single grouping factor out of a fitted model.
#' @keywords internal
.model_parts <- function(model) {
  mf <- stats::model.frame(model)
  if (ncol(mf) < 2L) stop("Model needs at least one predictor.", call. = FALSE)
  list(y = .strip_labelled(mf[[1]]), g = factor(mf[[2]]),
       y_name = names(mf)[1], g_name = names(mf)[2],
       y_label = .nice_label(mf[[1]], names(mf)[1]),
       g_label = .nice_label(mf[[2]], names(mf)[2]),
       n = nrow(mf))
}

#' MS_W and its df, straight off the fitted model.
#' @keywords internal
.error_term <- function(model) {
  tab <- stats::anova(model)
  list(MS_W = tab["Residuals", "Mean Sq"], df_W = tab["Residuals", "Df"])
}

#' Reshape long repeated-measures data to a subjects-by-levels matrix.
#'
#' Repeated-measures ANOVA needs complete cases, so subjects missing any level
#' are dropped. The count is returned rather than the drop happening silently.
#'
#' @keywords internal
.reshape_wide <- function(y, w, id) {
  w  <- factor(w)
  id <- factor(id)
  lv <- levels(w)

  cnt <- table(id, w)
  if (any(cnt > 1L)) {
    stop("Some subjects have more than one observation per level of the ",
         "within-subjects factor. anova_check() expects one row per ",
         "subject per condition.", call. = FALSE)
  }

  complete <- rownames(cnt)[rowSums(cnt == 1L) == length(lv)]
  n_dropped <- nlevels(id) - length(complete)
  if (length(complete) < 3L) {
    stop("Fewer than 3 subjects have data in every condition.", call. = FALSE)
  }

  keep <- id %in% complete
  wide <- matrix(NA_real_, nrow = length(complete), ncol = length(lv),
                 dimnames = list(complete, lv))
  idx  <- cbind(match(as.character(id[keep]), complete),
                match(as.character(w[keep]), lv))
  wide[idx] <- y[keep]

  list(wide = wide, n_dropped = n_dropped, keep = keep,
       ids = droplevels(id[keep]))
}

#' Greenhouse-Geisser and Huynh-Feldt epsilon.
#'
#' Computed directly from the covariance matrix of the repeated measures
#' rather than parsed out of R's printed anova heading, which is a display
#' artefact. Verified to reproduce the values R prints for
#' `anova(mlm, X = ~1, test = "Spherical")`.
#'
#' @keywords internal
.gg_hf <- function(wide) {
  n <- nrow(wide); p <- ncol(wide); k <- p - 1L
  M <- qr.Q(qr(cbind(1, stats::contr.helmert(p))))[, -1, drop = FALSE]
  A <- t(M) %*% stats::cov(wide) %*% M
  gg <- sum(diag(A))^2 / (k * sum(A * t(A)))
  hf <- (n * k * gg - 2) / (k * (n - 1 - k * gg))
  # The Huynh-Feldt estimator can exceed 1, which is impossible for an
  # epsilon, so it is capped. R prints the uncapped estimate in the heading
  # of anova(mlm, X = ~1, test = "Spherical") but uses the capped value for
  # the p value, so expect that heading to show a larger number sometimes.
  list(gg = gg, hf = min(hf, 1))
}

#' Sphericity, as a finding.
#' @keywords internal
.test_sphericity <- function(wide, alpha = 0.05) {

  p <- ncol(wide)
  if (p < 3L) {
    return(.finding("sphericity", "Sphericity", "untestable",
                    detail = "with two levels there is only one difference score, so sphericity holds by definition"))
  }

  eps <- .gg_hf(wide)
  mt  <- stats::mauchly.test(stats::lm(wide ~ 1), X = ~1)

  sev <- if (mt$p.value < alpha) {
    "violated"
  } else if (eps$gg < 0.9) {
    # Mauchly's test has poor power, especially at small n; a low epsilon
    # is worth flagging even when the test is non-significant.
    "watch"
  } else {
    "ok"
  }

  .finding("sphericity", "Sphericity", sev,
           statistic = unname(mt$statistic), p = mt$p.value, stat_name = "W",
           detail = sprintf("Greenhouse-Geisser eps = %.3f, Huynh-Feldt eps = %.3f%s",
                            eps$gg, eps$hf,
                            if (sev == "watch")
                              "; Mauchly is non-significant but epsilon is well below 1"
                            else ""))
}

#' Independence: flagged, never tested.
#' @keywords internal
.note_independence <- function() {
  .finding("independence", "Independence of observations", "untestable",
           detail = paste("a property of the design, not of the data;",
                          "check that no observation could have influenced another"))
}


# ---- Layer 2: design engines -----------------------------------------------

#' One-way between-subjects engine.
#' @keywords internal
.engine_oneway_bs <- function(model, center = "mean", alpha = 0.05) {
  mf <- stats::model.frame(model)
  y  <- mf[[1]]
  g  <- factor(mf[[2]])
  list(
    normality    = .test_normality(model, alpha),
    homogeneity  = .test_homogeneity(y, g, center, alpha),
    independence = .note_independence()
  )
}

#' One-way within-subjects engine.
#'
#' Normality is checked on the residuals of the additive model
#' `y ~ within + subject`, whose error term is the within-by-subject
#' interaction, i.e. exactly the error term of the repeated-measures F test.
#'
#' @keywords internal
.engine_oneway_ws <- function(model, wide, alpha = 0.05) {
  list(
    normality    = .test_normality(model, alpha),
    sphericity   = .test_sphericity(wide, alpha),
    independence = .finding(
      "independence", "Independence between subjects", "untestable",
      detail = paste("dependence WITHIN a subject is modelled here;",
                     "what is still assumed is that subjects are independent",
                     "of one another"))
  )
}

# .engine_factorial_bs() and .engine_mixed() are next. Each returns the same
# named list of findings; nothing above or below this layer changes when
# they land.


# ---- Layer 4: recommendation rules -----------------------------------------
#
# A rule is a predicate over the findings plus the advice it implies.
# Rules are tried in order; the first match wins. Keeping them here, as
# data, means "what to do about unequal variances" is editable in one
# place without touching Levene's test.

.anova_rules_oneway_bs <- list(

  list(
    when = function(f, ctx) f$homogeneity$severity == "violated" && !ctx$balanced,
    what = "Use Welch's ANOVA: oneway.test(y ~ group, data = d, var.equal = FALSE)",
    why  = paste("Variances differ and the groups are unequal in size. This is",
                 "exactly the case MDK single out: with unequal n the pooled error",
                 "term is dominated by the larger groups, and the true Type I error",
                 "rate departs from alpha in either direction depending on whether",
                 "the larger groups are the more or less variable ones."),
    cite = c("welch_anova", "homogeneity")
  ),

  list(
    when = function(f, ctx) f$homogeneity$severity == "violated" && ctx$balanced,
    what = "Proceed with aov(), and report oneway.test(var.equal = FALSE) as a sensitivity check",
    why  = paste("Variances differ, but the groups are equal in size, and with equal",
                 "n the standard F test is fairly robust to heterogeneity. MDK are",
                 "specific that Welch and Brown-Forsythe are preferable when sample",
                 "sizes are unequal AND variances are heterogeneous, and preferable",
                 "only when the population variances really do differ. Equal n puts",
                 "you outside the case they are worried about, so report both and",
                 "note that the conclusion does not hinge on the choice."),
    cite = c("homogeneity", "welch_anova")
  ),

  list(
    when = function(f, ctx) f$homogeneity$severity == "watch",
    what = "Proceed with aov(), but report the variance ratio and consider Welch's ANOVA as a sensitivity check",
    why  = paste("Levene's test is non-significant, but it has poor power at these",
                 "group sizes and the spread of variances is wide. Absence of",
                 "evidence here is weak evidence of absence."),
    cite = c("homogeneity", "welch_anova")
  ),

  list(
    when = function(f, ctx) f$normality$severity == "violated" && ctx$n_min < 15,
    what = "Fit a robust counterpart and compare: WRS2::t1way(y ~ group, data = d, tr = 0.2). Agree, report either; disagree, report the robust one",
    why  = paste("The residuals depart from normality and the groups are small",
                 "enough that the central limit theorem is not helping much. Note",
                 "what NOT to reach for first. Field and Wilcox give six reasons",
                 "transformations are seldom worth it, chief among them that",
                 "transforming changes the hypothesis and that distributions",
                 "usually stay skewed anyway. Trimming at 2.5 SD and then running",
                 "OLS is worse still: the SD is itself pulled by the outliers, so",
                 "they get masked, and the standard errors afterwards are wrong at",
                 "any sample size. A 20 percent trimmed-means ANOVA sidesteps both",
                 "problems. Look at the Q-Q plot too: a few extreme points is a",
                 "different problem from genuine skew."),
    cite = c("normality", "trimmed_means", "transformation", "outliers", "robust_sensitivity")
  ),

  list(
    when = function(f, ctx) f$normality$severity == "violated",
    what = "Inspect the Q-Q plot for tail weight, then run WRS2::t1way(tr = 0.2) as a sensitivity check alongside aov()",
    why  = paste("The residuals depart from normality. Group sizes here are large",
                 "enough that the sampling distribution of the mean is not the main",
                 "worry, but heavy tails still cost power and distort effect sizes,",
                 "and Field and Wilcox are pointed that blanket claims of ANOVA",
                 "being robust do not survive the evidence. Check whether the",
                 "departure is skew or heavy tails, and report the robust fit",
                 "beside the conventional one."),
    cite = c("normality", "normality_prevalence", "trimmed_means", "robust_sensitivity")
  ),

  list(
    when = function(f, ctx) f$normality$severity == "watch",
    what = "Proceed with aov()",
    why  = paste("Shapiro-Wilk is significant, but at this sample size it detects",
                 "departures far too small to affect the F test. Trust the Q-Q plot",
                 "over the p value here."),
    cite = c("normality")
  ),

  list(
    when = function(f, ctx) TRUE,
    what = "Proceed with the standard one-way ANOVA: aov(y ~ group, data = d)",
    why  = paste("Nothing in the testable assumptions is flagged. Independence",
                 "remains a design question rather than something the data can",
                 "confirm."),
    cite = c("model_comparison", "ms_within")
  )
)

.anova_rules_oneway_ws <- list(

  list(
    when = function(f, ctx) f$sphericity$severity == "violated" && ctx$gg < 0.75,
    what = "Report the Greenhouse-Geisser corrected test: anova(lm(wide ~ 1), X = ~1, test = \"Spherical\"), G-G Pr column",
    why  = paste("Sphericity is violated and epsilon is well below 1, which",
                 "inflates the Type I error rate of the uncorrected F. Both",
                 "degrees of freedom get multiplied by epsilon. Greenhouse-",
                 "Geisser is the conservative choice and is the usual",
                 "recommendation when epsilon is below about .75."),
    cite = c("sphericity", "sphericity_correction")
  ),

  list(
    when = function(f, ctx) f$sphericity$severity == "violated",
    what = "Report the Huynh-Feldt corrected test: anova(lm(wide ~ 1), X = ~1, test = \"Spherical\"), H-F Pr column",
    why  = paste("Sphericity is violated, but epsilon is above about .75, where",
                 "Greenhouse-Geisser over-corrects and costs power",
                 "unnecessarily. Huynh-Feldt is the less conservative",
                 "correction and is preferred in this range."),
    cite = c("sphericity", "sphericity_correction")
  ),

  list(
    when = function(f, ctx) f$sphericity$severity == "watch",
    what = "Report the uncorrected test, with the Greenhouse-Geisser corrected p as a sensitivity check",
    why  = paste("Mauchly's test is non-significant, but it has poor power at",
                 "these sample sizes and epsilon is noticeably below 1. Absence",
                 "of evidence is weak evidence of absence here, and reporting",
                 "both costs nothing."),
    cite = c("sphericity", "sphericity_correction")
  ),

  list(
    when = function(f, ctx) f$normality$severity %in% c("violated", "watch"),
    what = "Fit a robust counterpart and compare: WRS2::rmanova(y, groups, blocks, tr = 0.2)",
    why  = paste("The residuals depart from normality. As with between-subjects",
                 "designs, the answer is not a transformation: Field and Wilcox",
                 "give six reasons those are seldom worth it. A trimmed-means",
                 "repeated-measures test is the sensitivity analysis to run."),
    cite = c("normality", "trimmed_means", "robust_sensitivity")
  ),

  list(
    when = function(f, ctx) TRUE,
    what = "Proceed with the uncorrected repeated-measures ANOVA: aov(y ~ within + Error(id/within), data = d)",
    why  = paste("Nothing in the testable assumptions is flagged. Independence",
                 "between subjects remains a design question."),
    cite = c("model_comparison", "sphericity")
  )
)

#' Apply the rule table to a set of findings.
#' @keywords internal
.recommend <- function(findings, ctx, rules) {
  for (r in rules) {
    if (isTRUE(r$when(findings, ctx))) {
      return(list(what = r$what, why = r$why, cite = r$cite))
    }
  }
  NULL
}


# ---- Layer 1: orchestration ------------------------------------------------

#' Check the assumptions of an ANOVA model
#'
#' Runs the assumption checks appropriate to the design and recommends a next
#' step with a citation. Supports one-way between-subjects and one-way
#' within-subjects (repeated measures) designs.
#'
#' Normality is tested on the model's **residuals**, not the raw scores. The
#' raw values pool conditions with different means and can look non-normal
#' when every condition is perfectly normal.
#'
#' **Within-subjects designs take long-format data**, one row per subject per
#' condition, which is the format R wants everywhere else too. Pass the subject
#' identifier as `id` and nothing needs reshaping; the wide matrix needed for
#' the sphericity test is built internally. Subjects missing any condition are
#' dropped, and the count is reported.
#'
#' @param object A fitted `aov()` or `lm()`, or a two-sided formula.
#' @param data A data frame. Required when `object` is a formula.
#' @param id Name of the subject identifier column. Supplying it is what makes
#'   the design within-subjects. Requires the formula interface.
#' @param design `"auto"` infers the design. `"oneway_bs"` and `"oneway_ws"`
#'   force it.
#' @param center Centre for Levene's test: `"mean"` (classic Levene) or
#'   `"median"` (Brown-Forsythe, more robust to skew).
#' @param alpha Threshold for the printed verdicts. Default `.05`.
#'
#' @return An object of class `anova_check`.
#'
#' @examples
#' anova_check(aov(weight ~ group, data = PlantGrowth))
#' anova_check(weight ~ group, data = PlantGrowth)
#'
#' \dontrun{
#' # within-subjects, long format
#' anova_check(rt ~ condition, data = d, id = "participant")
#' }
#'
#' @seealso [anova_ref()] for the reasoning behind any concept cited in the
#'   output, [anova_check_plots()] for the diagnostic plots.
#' @importFrom stats aov model.frame residuals shapiro.test var median anova lm
#'   cov mauchly.test contr.helmert formula
#' @export
anova_check <- function(object, data = NULL, id = NULL,
                        design = c("auto", "oneway_bs", "oneway_ws"),
                        center = c("mean", "median"),
                        alpha = 0.05) {

  design <- match.arg(design)
  center <- match.arg(center)

  within <- !is.null(id) || design == "oneway_ws"

  if (within) {

    if (!inherits(object, "formula")) {
      stop("Within-subjects designs need the formula interface: ",
           "anova_check(dv ~ within, data = d, id = \"subject\").", call. = FALSE)
    }
    if (is.null(data)) stop("`data` is required when `object` is a formula.", call. = FALSE)
    if (is.null(id))   stop("Supply `id`, the subject identifier column.", call. = FALSE)
    if (!id %in% names(data)) {
      stop("No column called \"", id, "\" in `data`.", call. = FALSE)
    }

    mf <- stats::model.frame(object, data = data, na.action = stats::na.pass)
    if (ncol(mf) != 2L) {
      stop("Only one-way within-subjects designs are supported so far.", call. = FALSE)
    }
    y <- mf[[1]]
    w <- factor(mf[[2]])

    rs   <- .reshape_wide(y, w, data[[id]])
    wide <- rs$wide

    # The additive model y ~ within + subject has the within-by-subject
    # interaction as its error term, which is precisely the error term of
    # the repeated-measures F test. Its residuals are the right ones to
    # check for normality, and anova() on it reproduces the RM F exactly.
    long  <- data.frame(y = y[rs$keep], w = droplevels(w[rs$keep]),
                        subj = rs$ids)
    model <- stats::lm(y ~ w + subj, data = long)

    eps <- if (ncol(wide) >= 3L) .gg_hf(wide) else list(gg = 1, hf = 1)

    desc <- data.frame(
      condition = colnames(wide),
      n         = as.vector(colSums(!is.na(wide))),
      mean      = as.vector(colMeans(wide)),
      sd        = as.vector(apply(wide, 2, stats::sd)),
      stringsAsFactors = FALSE
    )
    names(desc)[1] <- names(mf)[2]

    ctx <- list(n = nrow(long), n_subj = nrow(wide), a = ncol(wide),
                balanced = TRUE, n_min = nrow(wide),
                gg = eps$gg, hf = eps$hf, n_dropped = rs$n_dropped)

    findings <- .engine_oneway_ws(model, wide, alpha)
    rec      <- .recommend(findings, ctx, .anova_rules_oneway_ws)

    return(structure(
      list(design = "oneway_ws", formula = object,
           y_name = names(mf)[1], g_name = names(mf)[2], id_name = id,
           context = ctx, descriptives = desc,
           findings = findings, recommendation = rec,
           alpha = alpha, center = center,
           model = model, wide = wide, epsilon = eps),
      class = "anova_check"))
  }

  # ---- between-subjects ----
  if (inherits(object, "formula")) {
    if (is.null(data)) stop("`data` is required when `object` is a formula.", call. = FALSE)
    model <- stats::aov(object, data = data)
  } else if (inherits(object, c("aov", "lm"))) {
    model <- object
  } else {
    stop("`object` must be a fitted aov()/lm() or a formula.", call. = FALSE)
  }

  mf <- stats::model.frame(model)
  if (ncol(mf) != 2L) {
    stop("Only one-way designs are supported so far. ",
         "Factorial and mixed engines are coming.", call. = FALSE)
  }

  y <- mf[[1]]
  g <- factor(mf[[2]])

  desc <- data.frame(
    group = levels(g),
    n     = as.vector(tapply(y, g, length)),
    mean  = as.vector(tapply(y, g, mean)),
    sd    = as.vector(tapply(y, g, stats::sd)),
    stringsAsFactors = FALSE
  )
  names(desc)[1] <- names(mf)[2]

  ctx <- list(n = nrow(mf), n_min = min(desc$n),
              balanced = length(unique(desc$n)) == 1L, a = nlevels(g))

  findings <- .engine_oneway_bs(model, center, alpha)
  rec      <- .recommend(findings, ctx, .anova_rules_oneway_bs)

  structure(
    list(design = "oneway_bs", formula = stats::formula(model),
         y_name = names(mf)[1], g_name = names(mf)[2],
         context = ctx, descriptives = desc,
         findings = findings, recommendation = rec,
         alpha = alpha, center = center, model = model),
    class = "anova_check")
}


# ---- Layer 5: presentation -------------------------------------------------

#' @keywords internal
.severity_tag <- function(s) {
  switch(s, ok = "OK", watch = "WATCH", violated = "VIOLATED",
         untestable = "not testable", s)
}

#' Print an assumption check
#'
#' @param x An `anova_check` object.
#' @param verbosity `"long"` (default) includes the reasoning and citations;
#'   `"short"` gives the verdict lines and the recommended next step only.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.anova_check <- function(x, verbosity = c("long", "short"), ...) {

  verbosity <- match.arg(verbosity)
  title <- paste("Assumption checks:", deparse(x$formula))
  cat("\n", title, "\n", strrep("=", nchar(title)), "\n\n", sep = "")

  if (identical(x$design, "oneway_ws")) {
    cat(sprintf("  %d subjects x %d conditions, within-subjects\n",
                x$context$n_subj, x$context$a))
    if (x$context$n_dropped > 0) {
      cat(sprintf("  %d subject(s) dropped for missing a condition\n",
                  x$context$n_dropped))
    }
    cat("\n")
  } else {
    cat(sprintf("  %d observations, %d groups (%s), %s\n\n",
                x$context$n, x$context$a,
                paste(x$descriptives$n, collapse = " / "),
                if (x$context$balanced) "balanced" else "UNBALANCED"))
  }

  for (f in x$findings) {
    stat <- if (is.na(f$statistic)) {
      ""
    } else if (length(f$df) == 2 && !any(is.na(f$df))) {
      sprintf("%s(%d, %d) = %.2f, p %s", f$stat_name, f$df[1], f$df[2],
              f$statistic, .p_phrase(f$p))
    } else {
      sprintf("%s = %.3f, p %s", f$stat_name, f$statistic, .p_phrase(f$p))
    }
    cat(sprintf("  %-26s %-34s %s\n", f$label, stat, .severity_tag(f$severity)))
    if (!is.na(f$detail)) {
      for (ln in strwrap(f$detail, width = 62)) cat("      ", ln, "\n", sep = "")
    }
  }

  if (!is.null(x$recommendation)) {
    cat("\n", strrep("-", nchar(title)), "\n", sep = "")
    cat("\n  RECOMMENDED NEXT STEP\n\n")
    for (ln in strwrap(x$recommendation$what, width = 68)) cat("    ", ln, "\n", sep = "")
    if (verbosity == "long") {
      cat("\n")
      for (ln in strwrap(x$recommendation$why, width = 68)) cat("      ", ln, "\n", sep = "")
      cat("\n    See: ", paste(x$recommendation$cite, collapse = ", "),
          "\n    (anova_ref(\"", x$recommendation$cite[1],
          "\") for the citation and reasoning)\n", sep = "")
    }
  }

  cat("\n  anova_check_plots() for the Q-Q, residual histogram, and boxplot.\n\n")
  invisible(x)
}

#' Format a p value for a sentence, reusing the package formatter.
#' @keywords internal
.p_phrase <- function(p) {
  if (is.na(p)) return("= NA")
  fp <- format_p(p)
  if (identical(fp, "< 0.001")) "< .001" else paste0("= ", sub("^0", "", fp))
}
