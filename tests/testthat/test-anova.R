# Tests for the anova_* family.
#
# Fixtures are PlantGrowth (ships with R) and seeded simulations, so these
# run anywhere without shipping data files.
#
# Optional packages are skipped rather than assumed: car is used only to
# cross-check Levene's test, WRS2 only for the robust specifications, and
# ggplot2 only for the plot constructors.


# ---- anova_check(), between-subjects ---------------------------------------

test_that("anova_check() returns a well-formed object", {
  chk <- anova_check(aov(weight ~ group, data = PlantGrowth))

  expect_s3_class(chk, "anova_check")
  expect_identical(chk$design, "oneway_bs")
  expect_named(chk$findings, c("normality", "homogeneity", "independence"))
  expect_true(all(vapply(chk$findings, inherits, logical(1), "anova_finding")))
  expect_true(chk$context$balanced)
  expect_identical(chk$context$a, 3L)
  expect_identical(chk$context$n, 30L)
})

test_that("anova_check() accepts a formula and a fitted model identically", {
  a <- anova_check(aov(weight ~ group, data = PlantGrowth))
  b <- anova_check(weight ~ group, data = PlantGrowth)
  expect_equal(a$findings$homogeneity$p, b$findings$homogeneity$p)
  expect_equal(a$findings$normality$statistic, b$findings$normality$statistic)
})

test_that("anova_check() rejects bad input", {
  expect_error(anova_check(weight ~ group), "data.*required")
  expect_error(anova_check("not a model"), "fitted aov")
  expect_error(anova_check(aov(breaks ~ wool + tension, data = warpbreaks)),
               "one-way")
})


# ---- the individual tests --------------------------------------------------

test_that("Levene's test reproduces known values on PlantGrowth", {
  chk <- anova_check(weight ~ group, data = PlantGrowth)
  expect_equal(chk$findings$homogeneity$statistic, 1.2369629545, tolerance = 1e-8)
  expect_equal(chk$findings$homogeneity$p,         0.3061949230, tolerance = 1e-8)
  expect_identical(chk$findings$homogeneity$severity, "ok")
})

test_that("Levene's test matches car::leveneTest() for both centres", {
  skip_if_not_installed("car")
  for (ctr in c("mean", "median")) {
    mine <- .levene(PlantGrowth$weight, PlantGrowth$group, ctr)
    theirs <- car::leveneTest(weight ~ group, data = PlantGrowth,
                              center = if (ctr == "mean") mean else stats::median)
    expect_equal(mine$p.value, theirs$`Pr(>F)`[1], tolerance = 1e-10,
                 info = paste("center =", ctr))
    expect_equal(mine$statistic, theirs$`F value`[1], tolerance = 1e-10,
                 info = paste("center =", ctr))
  }
})

test_that("normality is tested on residuals, not raw scores", {
  chk <- anova_check(weight ~ group, data = PlantGrowth)
  fit <- aov(weight ~ group, data = PlantGrowth)
  expect_equal(chk$findings$normality$statistic,
               unname(shapiro.test(residuals(fit))$statistic), tolerance = 1e-10)
  # and is NOT the raw-score result
  expect_false(isTRUE(all.equal(chk$findings$normality$statistic,
                                unname(shapiro.test(PlantGrowth$weight)$statistic))))
})

test_that("independence is reported as untestable, never as passing", {
  chk <- anova_check(weight ~ group, data = PlantGrowth)
  expect_identical(chk$findings$independence$severity, "untestable")
  expect_true(is.na(chk$findings$independence$p))
})

test_that("a large variance ratio is flagged even when Levene is non-significant", {
  # small groups, wide spread: Levene has little power here
  set.seed(1)
  d <- data.frame(g = factor(rep(c("a", "b"), each = 6)),
                  y = c(rnorm(6, 0, 1), rnorm(6, 0, 5)))
  f <- .test_homogeneity(d$y, d$g)
  expect_gt(f$p, 0.05)
  expect_identical(f$severity, "watch")
  expect_match(f$detail, "largest / smallest")
})


# ---- the recommendation rules ----------------------------------------------

test_that("clean data recommends the standard ANOVA", {
  chk <- anova_check(weight ~ group, data = PlantGrowth)
  expect_match(chk$recommendation$what, "standard one-way ANOVA")
})

test_that("unequal variances with unequal n recommends Welch", {
  set.seed(11)
  d <- data.frame(g = factor(rep(c("a", "b", "c"), times = c(30, 10, 10))),
                  y = c(rnorm(30, 10, 1), rnorm(10, 11, 6), rnorm(10, 12, 6)))
  chk <- anova_check(y ~ g, data = d)
  expect_identical(chk$findings$homogeneity$severity, "violated")
  expect_false(chk$context$balanced)
  expect_match(chk$recommendation$what, "Welch")
  expect_true("welch_anova" %in% chk$recommendation$cite)
})

test_that("unequal variances with equal n does not over-recommend Welch", {
  # MDK are specific that Welch is for unequal n AND unequal variances
  set.seed(12)
  d <- data.frame(g = factor(rep(c("a", "b", "c"), each = 20)),
                  y = c(rnorm(20, 10, 1), rnorm(20, 11, 6), rnorm(20, 12, 6)))
  chk <- anova_check(y ~ g, data = d)
  expect_identical(chk$findings$homogeneity$severity, "violated")
  expect_true(chk$context$balanced)
  expect_match(chk$recommendation$what, "sensitivity check")
})

test_that("non-normal residuals recommend a robust counterpart, not a transform", {
  set.seed(13)
  d <- data.frame(g = factor(rep(c("a", "b", "c"), each = 10)),
                  y = c(rexp(10, .2), rexp(10, .2) + 1, rexp(10, .2) + 2))
  chk <- anova_check(y ~ g, data = d)
  expect_identical(chk$findings$normality$severity, "violated")
  expect_match(chk$recommendation$what, "WRS2|robust")
  expect_true("trimmed_means" %in% chk$recommendation$cite)
})


# ---- anova_report() --------------------------------------------------------

test_that("anova_report() reproduces the ANOVA table and effect sizes", {
  r <- anova_report(aov(weight ~ group, data = PlantGrowth))

  expect_equal(r$F,      4.8460878624, tolerance = 1e-8)
  expect_identical(r$df1, 2L)
  expect_identical(r$df2, 27L)
  expect_equal(r$p,      0.0159099583, tolerance = 1e-8)
  expect_equal(r$r2,     0.2641482968, tolerance = 1e-8)
  expect_equal(r$omega2, 0.2040788460, tolerance = 1e-8)
  expect_lt(r$omega2, r$r2)                     # omega2 corrects R2 downward
  expect_match(r$apa, "^F\\(2, 27\\) = 4\\.85")
})

test_that("omega squared is clamped at zero rather than reported negative", {
  set.seed(3)
  d <- data.frame(g = factor(rep(c("a", "b", "c"), each = 8)), y = rnorm(24))
  r <- anova_report(aov(y ~ g, data = d))
  expect_gte(r$omega2, 0)
})

test_that("anova_report() refuses models it cannot compute omega squared for", {
  set.seed(101); n <- 10
  long <- data.frame(id = factor(rep(seq_len(n), 3)),
                     w  = factor(rep(c("a", "b", "c"), each = n)),
                     y  = rnorm(3 * n))
  expect_error(anova_report(lm(y ~ w + id, data = long)),
               "one-way between-subjects")
  expect_error(anova_report(lm(y ~ w + id, data = long)),
               "generalized eta squared")
})


# ---- anova_means() ---------------------------------------------------------

test_that("pooled standard errors come from the model, not the group SDs", {
  fit <- aov(weight ~ group, data = PlantGrowth)
  m   <- anova_means(fit)
  tab <- anova(fit)

  expect_equal(m$se, sqrt(tab["Residuals", "Mean Sq"] / m$n), tolerance = 1e-10)
  expect_equal(length(unique(round(m$se, 10))), 1L)   # balanced -> all equal
  expect_equal(m$se[1], 0.1971283658, tolerance = 1e-8)
  expect_match(attr(m, "se_type"), "pooled")
})

test_that("pooled = FALSE gives each group its own SE and df", {
  fit <- aov(weight ~ group, data = PlantGrowth)
  m   <- anova_means(fit, pooled = FALSE)

  expect_equal(m$se, m$sd / sqrt(m$n), tolerance = 1e-10)
  expect_equal(length(unique(round(m$se, 10))), 3L)
  expect_equal(m$se, c(0.1843896840, 0.2509822924, 0.1399539607), tolerance = 1e-8)
  expect_match(attr(m, "se_type"), "per group")
})

test_that("confidence intervals widen with the confidence level", {
  fit <- aov(weight ~ group, data = PlantGrowth)
  w95 <- with(anova_means(fit, conf = 0.95), ci_high - ci_low)
  w99 <- with(anova_means(fit, conf = 0.99), ci_high - ci_low)
  expect_true(all(w99 > w95))
})


# ---- within-subjects -------------------------------------------------------

make_ws <- function(seed = 101, n = 22) {
  set.seed(seed)
  subj <- rnorm(n, 0, 6)
  data.frame(
    participant = factor(rep(seq_len(n), 3)),
    condition   = factor(rep(c("neutral", "threat", "reward"), each = n),
                         levels = c("neutral", "threat", "reward")),
    rt = c(subj + rnorm(n, 500, 25), subj + rnorm(n, 545, 25),
           subj + rnorm(n, 520, 25)))
}

test_that("supplying id switches to the within-subjects engine", {
  ws <- anova_check(rt ~ condition, data = make_ws(), id = "participant")

  expect_identical(ws$design, "oneway_ws")
  expect_named(ws$findings, c("normality", "sphericity", "independence"))
  expect_identical(ws$context$n_subj, 22L)
  expect_identical(ws$context$n_dropped, 0L)
  expect_equal(dim(ws$wide), c(22L, 3L))
})

test_that("within-subjects requires the formula interface and a real id column", {
  d <- make_ws()
  expect_error(anova_check(aov(rt ~ condition, data = d), id = "participant"),
               "formula interface")
  expect_error(anova_check(rt ~ condition, data = d, id = "nope"),
               "No column called")
})

test_that("Greenhouse-Geisser and Huynh-Feldt match R's own anova.mlm", {
  ws  <- anova_check(rt ~ condition, data = make_ws(), id = "participant")
  hdr <- attr(anova(lm(ws$wide ~ 1), X = ~1, test = "Spherical"), "heading")
  gg  <- as.numeric(sub(".*Greenhouse-Geisser epsilon:\\s*([0-9.]+).*", "\\1",
                        paste(hdr, collapse = " ")))
  expect_equal(round(ws$epsilon$gg, 4), gg, tolerance = 1e-4)
  expect_lte(ws$epsilon$hf, 1)          # HF is capped; an epsilon cannot exceed 1
  expect_gte(ws$epsilon$hf, ws$epsilon$gg)
})

test_that("the repeated-measures F matches aov() with an Error term", {
  d   <- make_ws()
  ws  <- anova_check(rt ~ condition, data = d, id = "participant")
  mlm <- anova(lm(ws$wide ~ 1), X = ~1, test = "Spherical")
  av  <- summary(aov(rt ~ condition + Error(participant / condition), data = d))[[2]][[1]]

  expect_equal(mlm[1, "F"],      av[1, "F value"], tolerance = 1e-9)
  expect_equal(mlm[1, "num Df"], av[1, "Df"])
  expect_equal(mlm[1, "den Df"], av[2, "Df"])
})

test_that("subjects missing a condition are dropped and counted", {
  d  <- make_ws()
  ws <- anova_check(rt ~ condition, data = d[-c(1, 25, 47), ], id = "participant")
  expect_identical(ws$context$n_dropped, 2L)
  expect_identical(ws$context$n_subj, 20L)
})

test_that("duplicate rows per cell are an error, not silently averaged", {
  d <- make_ws()
  expect_error(anova_check(rt ~ condition, data = rbind(d, d[1, ]),
                           id = "participant"),
               "more than one observation")
})

test_that("with two levels sphericity is untestable rather than passing", {
  d  <- make_ws()
  d2 <- droplevels(d[d$condition != "reward", ])
  ws <- anova_check(rt ~ condition, data = d2, id = "participant")
  expect_identical(ws$findings$sphericity$severity, "untestable")
  expect_match(ws$findings$sphericity$detail, "by definition")
})

test_that("violated sphericity recommends the right correction", {
  set.seed(9); n <- 20; s <- rnorm(n, 0, 5)
  d <- data.frame(
    participant = factor(rep(seq_len(n), 4)),
    block = factor(rep(paste0("b", 1:4), each = n)),
    score = c(s + rnorm(n, 20, 2), s + rnorm(n, 22, 9),
              s + rnorm(n, 24, 2), s + rnorm(n, 25, 14)))
  ws <- anova_check(score ~ block, data = d, id = "participant")

  expect_identical(ws$findings$sphericity$severity, "violated")
  expect_lt(ws$epsilon$gg, 0.75)
  expect_match(ws$recommendation$what, "Greenhouse-Geisser")
  expect_true("sphericity_correction" %in% ws$recommendation$cite)
})


# ---- anova_multi() ---------------------------------------------------------

test_that("the specification grid is built and redundant cells collapsed", {
  specs <- .build_specs(list(transform = c("none", "log"),
                             variance  = c("pooled", "welch"),
                             estimator = c("ols", "trim20")))
  # 2 transforms x (2 OLS variance levels + 1 collapsed robust row) = 6
  expect_identical(nrow(specs), 6L)
  expect_true(all(specs$variance[specs$estimator == "trim20"] == "robust"))
  expect_identical(sum(specs$estimator == "trim20"), 2L)
})

test_that("anova_multi() fits every OLS specification", {
  mv <- anova_multi(weight ~ group, data = PlantGrowth,
                    contrast = c("trt2", "ctrl"),
                    decisions = list(transform = c("none", "log"),
                                     variance  = c("pooled", "welch"),
                                     estimator = "ols"))
  expect_s3_class(mv, "anova_multiverse")
  expect_identical(nrow(mv$results), 4L)
  expect_true(all(mv$results$ok))
  expect_true(all(mv$results$effect_type == "omega2"))
  expect_identical(mv$baseline$transform, "none")
  expect_identical(mv$baseline$variance, "pooled")
  expect_equal(mv$baseline$width_vs_base, 1)
})

test_that("the untransformed OLS specification equals the plain analysis", {
  mv <- anova_multi(weight ~ group, data = PlantGrowth,
                    contrast = c("trt2", "ctrl"),
                    decisions = list(transform = "none", variance = "pooled",
                                     estimator = "ols"))
  r <- anova_report(aov(weight ~ group, data = PlantGrowth))
  expect_equal(mv$results$F[1],         r$F,      tolerance = 1e-8)
  expect_equal(mv$results$p_omnibus[1], r$p,      tolerance = 1e-8)
  expect_equal(mv$results$effect[1],    r$omega2, tolerance = 1e-8)
})

test_that("impossible transformations are skipped with a stated reason", {
  set.seed(8)
  d <- data.frame(g = factor(rep(c("a", "b"), each = 15)),
                  y = c(rnorm(15, 0, 2), rnorm(15, 1.6, 2)))   # contains negatives
  mv <- anova_multi(y ~ g, data = d, contrast = c("b", "a"),
                    decisions = list(transform = c("none", "log", "sqrt"),
                                     variance = "pooled", estimator = "ols"))
  expect_true(mv$results$ok[mv$results$transform == "none"])
  expect_false(any(mv$results$ok[mv$results$transform != "none"]))
  expect_match(mv$results$why[mv$results$transform == "log"], "positive")
  expect_identical(mv$n_skipped, 2L)
})

test_that("anova_multi() validates the contrast", {
  expect_error(anova_multi(weight ~ group, data = PlantGrowth,
                           contrast = c("trt2", "nope")),
               "must name two levels")
})

test_that("robust specifications run when WRS2 is available", {
  skip_if_not_installed("WRS2")
  mv <- anova_multi(weight ~ group, data = PlantGrowth, contrast = c("trt2", "ctrl"))
  expect_identical(nrow(mv$results), 9L)
  expect_true(all(mv$results$ok))
  robust <- mv$results[mv$results$estimator == "trim20", ]
  expect_true(all(robust$effect_type == "xi"))
  expect_true(all(robust$ci_high > robust$ci_low))
})

test_that("disagreement with the baseline is detected", {
  skip_if_not_installed("WRS2")
  set.seed(8)
  d <- data.frame(g = factor(rep(c("ctrl", "trt"), each = 20)),
                  y = c(rlnorm(20, 2.0, .85), rlnorm(20, 2.45, .85)))
  mv <- anova_multi(y ~ g, data = d, contrast = c("trt", "ctrl"))
  expect_gt(mv$n_disagree, 0)
  expect_true(any(mv$results$disagrees & mv$results$estimator == "trim20"))
  expect_false(mv$baseline$disagrees)   # the baseline never disagrees with itself
})


# ---- the reference registry ------------------------------------------------

test_that("every registry entry is well-formed", {
  for (k in names(.anova_refs)) {
    e <- .anova_refs[[k]]
    expect_identical(e$concept, k, info = k)
    expect_true(nzchar(e$label), info = k)
    expect_type(e$verified, "logical")
    expect_true(is.na(e$gloss) || nzchar(e$gloss), info = k)
  }
})

test_that("a verified entry actually records where it came from", {
  for (k in names(.anova_refs)) {
    e <- .anova_refs[[k]]
    if (isTRUE(e$verified)) {
      expect_true(!is.na(e$mdk_ch) || length(e$also) > 0,
                  info = paste(k, "is marked verified but cites nothing"))
    }
  }
})

test_that("an unverified entry never claims an equation number", {
  # the honesty rule: a wrong equation number is worse than a missing one
  for (k in names(.anova_refs)) {
    e <- .anova_refs[[k]]
    if (!isTRUE(e$verified)) {
      expect_true(is.na(e$mdk_eq),
                  info = paste(k, "has an equation number but is not verified"))
    }
  }
})

test_that("every citation key used by a recommendation rule exists", {
  keys <- names(.anova_refs)
  for (tbl in list(.anova_rules_oneway_bs, .anova_rules_oneway_ws)) {
    for (rule in tbl) {
      missing <- setdiff(rule$cite, keys)
      expect_identical(missing, character(0),
                       info = paste("rule:", substr(rule$what, 1, 50),
                                    "| unknown key(s):", paste(missing, collapse = ", ")))
    }
  }
})

test_that("anova_ref() retrieves entries and rejects unknown keys", {
  e <- anova_ref("omega_squared", quiet = TRUE)
  expect_identical(e$mdk_eq, "96")
  expect_identical(e$mdk_ch, 3)
  expect_true(e$verified)

  expect_error(anova_ref("not_a_concept"), "Unknown concept")
  expect_type(anova_ref(), "character")
})

test_that("anova_textbook() returns the MDK citation", {
  expect_match(anova_textbook(), "Maxwell")
  expect_match(anova_textbook(), "3rd ed")
})


# ---- plots -----------------------------------------------------------------

test_that("plot constructors return ggplots", {
  skip_if_not_installed("ggplot2")
  fit <- aov(weight ~ group, data = PlantGrowth)
  chk <- anova_check(fit)

  p <- anova_check_plots(chk)
  expect_named(p, c("qq", "residuals", "boxplot"))
  expect_true(all(vapply(p, inherits, logical(1), "ggplot")))

  expect_length(anova_check_plots(fit), 3)     # also accepts a raw model
  expect_s3_class(anova_plot(fit), "ggplot")
  expect_s3_class(anova_plot(fit, points = FALSE, pooled = FALSE), "ggplot")
})

test_that("anova_multi_plot() returns a ggplot", {
  skip_if_not_installed("ggplot2")
  mv <- anova_multi(weight ~ group, data = PlantGrowth,
                    contrast = c("trt2", "ctrl"),
                    decisions = list(transform = "none",
                                     variance = c("pooled", "welch"),
                                     estimator = "ols"))
  expect_s3_class(anova_multi_plot(mv), "ggplot")
})

test_that("anova_check_plots() rejects input it cannot use", {
  skip_if_not_installed("ggplot2")
  expect_error(anova_check_plots("nope"), "anova_check object")
})


# ---- print methods ---------------------------------------------------------

test_that("print methods run and return their object invisibly", {
  fit <- aov(weight ~ group, data = PlantGrowth)

  chk <- anova_check(fit)
  expect_output(print(chk), "Assumption checks")
  expect_output(print(chk, verbosity = "short"), "RECOMMENDED NEXT STEP")
  expect_identical(withVisible(print(chk))$visible, FALSE)

  expect_output(print(anova_report(fit)), "omega\\^2")

  mv <- anova_multi(weight ~ group, data = PlantGrowth, contrast = c("trt2", "ctrl"),
                    decisions = list(transform = "none", variance = "pooled",
                                     estimator = "ols"))
  expect_output(print(mv), "Mini-multiverse")
})


# ---- themes ----------------------------------------------------------------

test_that(".is_dark_theme() identifies dark themes by name, function, and object", {
  skip_if_not_installed("ggplot2")
  expect_true(.is_dark_theme("dark"))
  expect_false(.is_dark_theme("jeremy"))
  expect_false(.is_dark_theme("gridline"))
  expect_false(.is_dark_theme("none"))

  expect_true(.is_dark_theme(theme_black()))          # by inspecting the object
  expect_false(.is_dark_theme(jermeys_theme()))
  expect_false(.is_dark_theme(ggplot2::theme_minimal()))
  expect_false(.is_dark_theme(ggplot2::theme_grey()))
})

test_that(".anova_theme() resolves names, objects, and functions", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(.anova_theme("jeremy"), "theme")
  expect_s3_class(.anova_theme("dark"), "theme")
  expect_s3_class(.anova_theme("gridline"), "theme")
  expect_null(.anova_theme("none"))

  expect_s3_class(.anova_theme(ggplot2::theme_minimal()), "theme")
  expect_s3_class(.anova_theme(jermeys_theme), "theme")
  expect_error(.anova_theme("nonsense"), "should be one of")
})

test_that("geom colours follow the theme rather than staying fixed", {
  # a black panel with black points would be unreadable, so the palette flips
  light <- .anova_palette(dark = FALSE)
  dark  <- .anova_palette(dark = TRUE)
  expect_identical(light$ink, "black")
  expect_identical(dark$ink,  "white")
  expect_false(identical(light$points, dark$points))
  expect_named(light, names(dark))
})

test_that("every plot function accepts every theme option", {
  skip_if_not_installed("ggplot2")
  fit <- aov(weight ~ group, data = PlantGrowth)
  chk <- anova_check(fit)

  for (th in c("jeremy", "dark", "gridline", "none")) {
    expect_s3_class(anova_plot(fit, theme = th), "ggplot")
    p <- anova_check_plots(chk, theme = th)
    expect_true(all(vapply(p, inherits, logical(1), "ggplot")), info = th)
  }

  # and a user-supplied theme object
  expect_s3_class(anova_plot(fit, theme = ggplot2::theme_minimal()), "ggplot")
})

test_that("anova_multi_plot() accepts the theme argument", {
  skip_if_not_installed("ggplot2")
  mv <- anova_multi(weight ~ group, data = PlantGrowth, contrast = c("trt2", "ctrl"),
                    decisions = list(transform = "none",
                                     variance = c("pooled", "welch"),
                                     estimator = "ols"))
  expect_s3_class(anova_multi_plot(mv, theme = "dark"), "ggplot")
  expect_s3_class(anova_multi_plot(mv, theme = "none"), "ggplot")
})

test_that("the bundled themes are free of deprecated ggplot2 arguments", {
  skip_if_not_installed("ggplot2")
  # size= in element_line() has been deprecated since ggplot2 3.4; building a
  # plot with one warns every time
  fit <- aov(weight ~ group, data = PlantGrowth)
  for (th in c("jeremy", "dark", "gridline")) {
    expect_no_warning(ggplot2::ggplot_build(anova_plot(fit, theme = th)))
  }
})


# ---- y axis ----------------------------------------------------------------

test_that(".y_limits() pads the data and CI range by 10% when unconstrained", {
  yl <- .y_limits(c(10, 20), ci_low = 12, ci_high = 18)
  expect_false(yl$exact)
  expect_equal(yl$data, c(10, 20))
  expect_equal(yl$lim, c(9, 21))          # span 10, padded 1 each side
})

test_that(".y_limits() includes the CIs, which can sit outside the data", {
  # a CI wider than the observed range must not be clipped off the panel
  yl <- .y_limits(c(10, 20), ci_low = 5, ci_high = 25)
  expect_equal(yl$full, c(5, 25))
  expect_lt(yl$lim[1], 5)
  expect_gt(yl$lim[2], 25)
})

test_that(".y_limits() uses an explicit range exactly, with no padding", {
  yl <- .y_limits(c(3, 6), ci_low = 3.5, ci_high = 5.5, y_range = c(1, 7))
  expect_true(yl$exact)
  expect_equal(yl$lim, c(1, 7))
  expect_equal(yl$data, c(3, 6))
})

test_that(".y_limits() sorts a reversed range and validates it", {
  expect_equal(.y_limits(c(3, 6), 3.5, 5.5, y_range = c(7, 1))$lim, c(1, 7))
  expect_error(.y_limits(c(3, 6), 3.5, 5.5, y_range = 5), "two numbers")
  expect_error(.y_limits(c(3, 6), 3.5, 5.5, y_range = c(1, NA)), "two numbers")
})

test_that(".y_limits() warns when the range would hide data", {
  expect_warning(.y_limits(c(3, 6), 3.5, 5.5, y_range = c(4, 5)),
                 "does not cover the data")
})

test_that(".y_limits() survives a constant outcome", {
  yl <- .y_limits(rep(5, 10), 5, 5)
  expect_true(yl$lim[2] > yl$lim[1])       # a zero-span axis would be invalid
})

test_that("limits zoom rather than drop observations", {
  skip_if_not_installed("ggplot2")
  fit <- aov(weight ~ group, data = PlantGrowth)

  # scale_y_continuous(limits=) would delete rows; coord_cartesian must not
  full <- ggplot2::ggplot_build(anova_plot(fit, quiet = TRUE))
  clip <- suppressWarnings(
    ggplot2::ggplot_build(anova_plot(fit, y_range = c(5, 6), quiet = TRUE)))
  expect_identical(nrow(full$data[[1]]), nrow(clip$data[[1]]))
  expect_gt(nrow(clip$data[[1]]), 0)
})

test_that("the axis note is attached and describes the decision", {
  skip_if_not_installed("ggplot2")
  fit <- aov(weight ~ group, data = PlantGrowth)

  p <- anova_plot(fit, quiet = TRUE)
  note <- attr(p, "anova_note")
  expect_type(note, "character")
  expect_match(note, "data ranged")
  expect_match(note, "plus 10% each side")
  expect_match(note, "coord_cartesian", fixed = TRUE)

  exact <- attr(anova_plot(fit, y_range = c(0, 8), quiet = TRUE), "anova_note")
  # the note is wrapped, so collapse whitespace before matching a phrase
  expect_match(gsub("[[:space:]]+", " ", exact), "exactly, as given by y_range")
  expect_match(exact, "c(0, 8)", fixed = TRUE)
})

test_that("quiet controls the message but not the attribute", {
  skip_if_not_installed("ggplot2")
  fit <- aov(weight ~ group, data = PlantGrowth)
  expect_message(anova_plot(fit), "Y axis")
  expect_no_message(anova_plot(fit, quiet = TRUE))
  expect_false(is.null(attr(anova_plot(fit, quiet = TRUE), "anova_note")))
})


# ---- points at scale -------------------------------------------------------

test_that(".point_style() thins the points as n grows", {
  small <- .point_style(30)
  mid   <- .point_style(150)
  large <- .point_style(400)

  expect_true(small$draw && mid$draw && large$draw)
  expect_gt(small$alpha, mid$alpha)
  expect_gt(mid$alpha, large$alpha)
  expect_gt(small$size, large$size)
})

test_that(".point_style() drops the points past the threshold on auto", {
  expect_true(.point_style(500)$draw)
  expect_false(.point_style(501)$draw)
  expect_match(.point_style(1500)$why, "too many to plot")
  expect_match(.point_style(1500)$why, "points = \"all\"", fixed = TRUE)
})

test_that('points = "all" overrides the threshold', {
  s <- .point_style(5000, "all")
  expect_true(s$draw)
  expect_false(s$auto)
  expect_true(is.na(s$why))
})

test_that('points = "none" never draws, at any n', {
  expect_false(.point_style(10, "none")$draw)
  expect_false(.point_style(10000, "none")$draw)
})

test_that("logical points is accepted for backwards compatibility", {
  expect_true(.point_style(30, TRUE)$draw)
  expect_false(.point_style(30, FALSE)$draw)
})

test_that("anova_plot() reports what it did with the points", {
  skip_if_not_installed("ggplot2")
  set.seed(6)
  big <- data.frame(g = factor(rep(c("a", "b"), each = 800)),
                    y = c(rnorm(800, 10, 2), rnorm(800, 11, 2)))
  fit <- aov(y ~ g, data = big)

  auto <- anova_plot(fit, quiet = TRUE)
  expect_match(attr(auto, "anova_note"), "too many to plot")
  # the jitter layer is absent, so only violin + errorbar + mean remain
  expect_length(auto$layers, 3)

  forced <- anova_plot(fit, points = "all", quiet = TRUE)
  expect_length(forced$layers, 4)
})

test_that("anova_plot() still accepts every points spelling", {
  skip_if_not_installed("ggplot2")
  fit <- aov(weight ~ group, data = PlantGrowth)
  for (pt in list("auto", "all", "none", TRUE, FALSE)) {
    expect_s3_class(anova_plot(fit, points = pt, quiet = TRUE), "ggplot")
  }
})


# ---- axis breaks -----------------------------------------------------------

test_that(".y_breaks() labels every point of a short whole-numbered scale", {
  expect_equal(.y_breaks(c(3, 5, 7), y_range = c(1, 7)), 1:7)
  expect_equal(.y_breaks(c(1, 2, 3), y_range = c(0, 10)), 0:10)
})

test_that(".y_breaks() leaves long or continuous scales to ggplot2", {
  expect_null(.y_breaks(c(40, 70), y_range = c(0, 100)))    # 100 points is too many
  expect_null(.y_breaks(c(1.5, 2.5), y_range = c(1.5, 7.5)))  # not whole numbers
  expect_null(.y_breaks(c(3.59, 6.31)))                      # continuous, undeclared
})

test_that(".y_breaks() infers a discrete scale that was not declared", {
  # forgetting y_range should still give sensible breaks for Likert-like data
  expect_equal(.y_breaks(c(3, 4, 5, 6, 7)), 3:7)
  expect_null(.y_breaks(seq(0, 100, by = 1)))               # whole, but too long
})

test_that("an explicit y_breaks overrides the inference", {
  skip_if_not_installed("ggplot2")
  set.seed(5)
  lik <- data.frame(cond = factor(rep(c("a", "b"), each = 30)),
                    rating = c(sample(3:6, 30, TRUE), sample(4:7, 30, TRUE)))
  p <- anova_plot(aov(rating ~ cond, data = lik), y_range = c(1, 7),
                  y_breaks = c(1, 4, 7), quiet = TRUE)
  drawn <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$breaks
  expect_true(all(c(1, 4, 7) %in% drawn))
  expect_false(2 %in% drawn)
})

test_that("Likert breaks reach the panel, and continuous ones are untouched", {
  skip_if_not_installed("ggplot2")
  set.seed(5)
  lik <- data.frame(cond = factor(rep(c("a", "b"), each = 30)),
                    rating = c(sample(3:6, 30, TRUE), sample(4:7, 30, TRUE)))

  drawn <- ggplot2::ggplot_build(
    anova_plot(aov(rating ~ cond, data = lik), y_range = c(1, 7), quiet = TRUE)
  )$layout$panel_params[[1]]$y$breaks
  expect_true(all(1:7 %in% drawn))

  # a continuous outcome keeps ggplot2's own choice
  cont <- ggplot2::ggplot_build(
    anova_plot(aov(weight ~ group, data = PlantGrowth), quiet = TRUE)
  )$layout$panel_params[[1]]$y$breaks
  expect_false(all(seq(3, 7) %in% stats::na.omit(cont)))
})

test_that("setting breaks does not drop observations", {
  skip_if_not_installed("ggplot2")
  set.seed(5)
  lik <- data.frame(cond = factor(rep(c("a", "b"), each = 30)),
                    rating = c(sample(3:6, 30, TRUE), sample(4:7, 30, TRUE)))
  b <- ggplot2::ggplot_build(anova_plot(aov(rating ~ cond, data = lik),
                                        y_range = c(1, 7), quiet = TRUE))
  expect_gt(nrow(b$data[[1]]), 0)
  expect_identical(nrow(b$data[[2]]), 60L)   # every observation still plotted
})


# ---- mean point size -------------------------------------------------------

test_that("the mean point stays full size when the CI is visible", {
  s <- .mean_point_size(ci_low = 4, ci_high = 6, ylim = c(0, 10))
  expect_equal(s$size, 4)
  expect_false(s$reduced)
})

test_that("the mean point shrinks when it would cover its own CI", {
  # a CI spanning 1% of the axis would sit entirely under a size-4 point
  s <- .mean_point_size(ci_low = 4.95, ci_high = 5.05, ylim = c(0, 10))
  expect_lt(s$size, 4)
  expect_true(s$reduced)
  expect_gte(s$size, 1.5)          # floored, never invisible
})

test_that("mean point size is driven by the CI-to-axis ratio, not n alone", {
  # same interval, wider axis -> smaller point
  narrow <- .mean_point_size(4, 6, ylim = c(0, 10))
  wide   <- .mean_point_size(4, 6, ylim = c(0, 500))
  expect_gt(narrow$size, wide$size)
})

test_that(".mean_point_size() copes with a degenerate axis", {
  s <- .mean_point_size(5, 5, ylim = c(5, 5))
  expect_equal(s$size, 4)
  expect_false(s$reduced)
})

test_that("anova_plot() shrinks the mean point at large n and says so", {
  skip_if_not_installed("ggplot2")
  set.seed(6)
  big <- data.frame(g = factor(rep(c("a", "b"), each = 1200)),
                    y = c(rnorm(1200, 10, 2), rnorm(1200, 10.6, 2)))
  note <- attr(anova_plot(aov(y ~ g, data = big), quiet = TRUE), "anova_note")
  expect_match(note, "Mean point: shrunk")

  small <- attr(anova_plot(aov(weight ~ group, data = PlantGrowth), quiet = TRUE),
                "anova_note")
  expect_no_match(small, "Mean point: shrunk")
})


# ---- axis labels from the label attribute ----------------------------------

labelled_pg <- function() {
  d <- PlantGrowth
  attr(d$weight, "label") <- "Dried plant weight (g)"
  attr(d$group,  "label") <- "Treatment condition"
  d
}

test_that(".nice_label() prefers the label attribute and falls back cleanly", {
  x <- 1:5
  expect_identical(.nice_label(x, "weight"), "weight")

  attr(x, "label") <- "Dried weight"
  expect_identical(.nice_label(x, "weight"), "Dried weight")

  attr(x, "label") <- ""                 # empty is not a usable label
  expect_identical(.nice_label(x, "weight"), "weight")

  attr(x, "label") <- c("a", "b")        # nor is a vector
  expect_identical(.nice_label(x, "weight"), "weight")

  attr(x, "label") <- 42                 # nor a number
  expect_identical(.nice_label(x, "weight"), "weight")
})

test_that(".model_parts() carries labels through the model frame", {
  p <- .model_parts(aov(weight ~ group, data = labelled_pg()))
  expect_identical(p$y_label, "Dried plant weight (g)")
  expect_identical(p$g_label, "Treatment condition")
  expect_identical(p$y_name, "weight")     # the name is still available
  expect_identical(p$g_name, "group")

  plain <- .model_parts(aov(weight ~ group, data = PlantGrowth))
  expect_identical(plain$y_label, "weight")
  expect_identical(plain$g_label, "group")
})

test_that("anova_plot() labels the axes from the attributes", {
  skip_if_not_installed("ggplot2")
  p <- anova_plot(aov(weight ~ group, data = labelled_pg()), quiet = TRUE)
  expect_identical(p$labels$y, "Dried plant weight (g)")
  expect_identical(p$labels$x, "Treatment condition")
})

test_that("explicit xlab and ylab still win", {
  skip_if_not_installed("ggplot2")
  p <- anova_plot(aov(weight ~ group, data = labelled_pg()),
                  xlab = "MINE X", ylab = "MINE Y", quiet = TRUE)
  expect_identical(p$labels$x, "MINE X")
  expect_identical(p$labels$y, "MINE Y")
})

test_that("anova_check_plots() uses the labels too", {
  skip_if_not_installed("ggplot2")
  p <- anova_check_plots(anova_check(aov(weight ~ group, data = labelled_pg())))
  expect_identical(p$boxplot$labels$x, "Treatment condition")
  expect_identical(p$boxplot$labels$y, "Dried plant weight (g)")
})

test_that("a haven_labelled outcome plots rather than erroring", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("haven")
  d <- PlantGrowth
  d$weight <- haven::labelled(d$weight, label = "Weight via haven")

  # the vctrs class must be stripped before it reaches a ggplot2 scale
  expect_false(inherits(.strip_labelled(d$weight), "vctrs_vctr"))

  p <- anova_plot(aov(weight ~ group, data = d), quiet = TRUE)
  expect_identical(p$labels$y, "Weight via haven")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})


# ---- colour ----------------------------------------------------------------

test_that(".group_colours() honours TRUE, FALSE, and a custom vector", {
  expect_null(.group_colours(FALSE, 3))
  expect_null(.group_colours(NULL, 3))
  expect_identical(.group_colours(TRUE, 3), jermeys_palette[1:3])
  expect_identical(.group_colours(c("red", "blue"), 2), c("red", "blue"))
})

test_that(".group_colours() recycles when there are more groups than colours", {
  n <- length(jermeys_palette) + 3
  cols <- .group_colours(TRUE, n)
  expect_length(cols, n)
  expect_identical(cols[1], cols[length(jermeys_palette) + 1])
})

test_that("colour = TRUE adds fill and colour scales, FALSE does not", {
  skip_if_not_installed("ggplot2")
  fit <- aov(weight ~ group, data = PlantGrowth)

  coloured <- anova_plot(fit, quiet = TRUE)
  grey     <- anova_plot(fit, colour = FALSE, quiet = TRUE)

  has_scale <- function(p, aes) {
    any(vapply(p$scales$scales,
               function(s) aes %in% s$aesthetics, logical(1)))
  }
  expect_true(has_scale(coloured, "fill"))
  expect_false(has_scale(grey, "fill"))
})

test_that("custom colours reach the plot", {
  skip_if_not_installed("ggplot2")
  mine <- c("#1b7837", "#762a83", "#e08214")
  p <- anova_plot(aov(weight ~ group, data = PlantGrowth),
                  colour = mine, quiet = TRUE)
  fills <- ggplot2::ggplot_build(p)$data[[1]]$fill
  expect_true(all(toupper(unique(fills)) %in% toupper(mine)))
})

test_that("colour works alongside every theme, including dark", {
  skip_if_not_installed("ggplot2")
  fit <- aov(weight ~ group, data = PlantGrowth)
  for (th in c("jeremy", "dark", "gridline", "none")) {
    expect_s3_class(
      ggplot2::ggplot_build(anova_plot(fit, theme = th, quiet = TRUE)),
      "ggplot_built")
  }
})

test_that("the mean and its interval stay in the theme ink, not the group colour", {
  skip_if_not_installed("ggplot2")
  fit <- aov(weight ~ group, data = PlantGrowth)

  light <- ggplot2::ggplot_build(anova_plot(fit, quiet = TRUE))
  dark  <- ggplot2::ggplot_build(anova_plot(fit, theme = "dark", quiet = TRUE))

  # last layer is the mean point
  expect_true(all(light$data[[length(light$data)]]$colour == "black"))
  expect_true(all(dark$data[[length(dark$data)]]$colour == "white"))
})


# ---- set_var_labels() / var_labels() ---------------------------------------

test_that("set_var_labels() attaches plain base R attributes", {
  d <- set_var_labels(PlantGrowth,
                      weight = "Dried plant weight (g)",
                      group  = "Treatment condition")

  expect_identical(attr(d$weight, "label"), "Dried plant weight (g)")
  expect_identical(attr(d$group,  "label"), "Treatment condition")
  # no class was added; it is still a plain numeric
  expect_identical(class(d$weight), "numeric")
  expect_false(inherits(d$weight, "haven_labelled"))
  expect_equal(as.vector(d$weight), PlantGrowth$weight)
})

test_that("labels can be supplied as a vector, and ... wins a clash", {
  lab <- c(weight = "From the vector", group = "Treatment condition")
  a <- set_var_labels(PlantGrowth, labels = lab)
  expect_identical(attr(a$weight, "label"), "From the vector")

  b <- set_var_labels(PlantGrowth, labels = lab, weight = "From the dots")
  expect_identical(attr(b$weight, "label"), "From the dots")
  expect_identical(attr(b$group,  "label"), "Treatment condition")
})

test_that("set_var_labels() validates its input", {
  expect_error(set_var_labels(1:5, a = "x"), "must be a data frame")
  expect_error(set_var_labels(PlantGrowth, "unnamed"), "must be named")
  expect_error(set_var_labels(PlantGrowth, weight = c("a", "b")),
               "single string")
  expect_error(set_var_labels(PlantGrowth, weight = 42), "single string")
  expect_error(set_var_labels(PlantGrowth, labels = c("no name")),
               "named vector")
})

test_that("set_var_labels() warns about columns that are not there", {
  expect_warning(set_var_labels(PlantGrowth, nope = "x"), "ignored: nope")
  expect_no_warning(set_var_labels(PlantGrowth, nope = "x", warn_missing = FALSE))
  # the real columns are still labelled despite the bad one
  d <- suppressWarnings(
    set_var_labels(PlantGrowth, nope = "x", weight = "Weight"))
  expect_identical(attr(d$weight, "label"), "Weight")
})

test_that("set_var_labels() with nothing to do returns the frame unchanged", {
  expect_identical(set_var_labels(PlantGrowth), PlantGrowth)
})

test_that("var_labels() reads them back, one element per column", {
  d <- set_var_labels(PlantGrowth, weight = "Dried plant weight (g)")
  v <- var_labels(d)

  expect_length(v, ncol(d))
  expect_named(v, names(d))
  expect_identical(unname(v["weight"]), "Dried plant weight (g)")
  expect_identical(unname(v["group"]), "group")        # falls back to the name
})

test_that('var_labels(missing = "na") shows what still needs labelling', {
  d <- set_var_labels(PlantGrowth, weight = "Dried plant weight (g)")
  v <- var_labels(d, missing = "na")
  expect_true(is.na(v[["group"]]))
  expect_identical(names(which(is.na(v))), "group")
})

test_that("var_labels() ignores unusable label attributes", {
  d <- PlantGrowth
  attr(d$weight, "label") <- ""            # empty
  attr(d$group,  "label") <- c("a", "b")   # not length 1
  v <- var_labels(d, missing = "na")
  expect_true(all(is.na(v)))
})

test_that("var_labels() validates its input", {
  expect_error(var_labels(1:5), "must be a data frame")
})


# ---- labels reach the plots ------------------------------------------------

test_that("set_var_labels() feeds anova_plot() without haven", {
  skip_if_not_installed("ggplot2")
  d <- set_var_labels(PlantGrowth,
                      weight = "Dried plant weight (g)",
                      group  = "Treatment condition")
  p <- anova_plot(aov(weight ~ group, data = d), quiet = TRUE)
  expect_identical(p$labels$y, "Dried plant weight (g)")
  expect_identical(p$labels$x, "Treatment condition")
})

test_that("labels survive the operations documented as safe", {
  skip_if_not_installed("dplyr")
  d <- set_var_labels(PlantGrowth, weight = "W")
  keep <- function(x) identical(attr(x, "label"), "W")

  expect_true(keep(dplyr::filter(d, weight > 4)$weight))
  expect_true(keep(dplyr::arrange(d, weight)$weight))
  expect_true(keep(dplyr::mutate(d, z = 1)$weight))
  expect_true(keep(dplyr::select(d, weight)$weight))
  expect_true(keep(rbind(d, d)$weight))
  expect_true(keep(stats::model.frame(aov(weight ~ group, data = d))[[1]]))

  f <- tempfile(fileext = ".rds"); on.exit(unlink(f))
  saveRDS(d, f)
  expect_true(keep(readRDS(f)$weight))
})

test_that("base R row subsetting drops labels, as documented", {
  # this is the surprising one, and the reason it is written down
  d <- set_var_labels(PlantGrowth, weight = "W")
  expect_null(attr(d[1:10, ]$weight, "label"))
  expect_null(attr(subset(d, weight > 4)$weight, "label"))
  expect_null(attr(c(d$weight, 1), "label"))
})
