# Tests for the reliability_helpers.R family: scale_reliability(),
# scale_reliabilities(), and their tidy_*() counterparts.
#
# Fixture is psych::bfi (bundled with psych, an Imports dependency, so it
# ships wherever this package does) subset down to a small, seeded sample of
# two five-item scales (Agreeableness, Conscientiousness) so the underlying
# unidim()/omega() calls stay fast and reproducible.
#
# psych is a hard Imports dependency, but skip_if_not_installed() is kept
# anyway to match this package's existing habit (see test-anova.R's dplyr
# block) of not assuming any package is present at test time.


make_bfi_subset <- function(seed = 1, n = 150) {
  d <- na.omit(psych::bfi)
  set.seed(seed)
  d[sample(nrow(d), n), ]
}

agree_items <- c("A1", "A2", "A3", "A4", "A5")
consc_items <- c("C1", "C2", "C3", "C4", "C5")

quietly <- function(expr) suppressWarnings(suppressMessages(expr))


# ---- scale_reliability(), ungrouped ----------------------------------------

test_that("scale_reliability() returns uni + total omega by default", {
  skip_if_not_installed("psych")
  d <- make_bfi_subset()

  rel <- quietly(scale_reliability(d, agree_items))

  expect_named(rel, c("uni", "omega"))
  expect_equal(unname(rel$uni$uni["u"]),     0.8571451244, tolerance = 1e-8)
  expect_equal(unname(rel$uni$uni["alpha"]), 0.7431845417, tolerance = 1e-8)
  expect_equal(rel$omega$omega.tot,          0.7585117140, tolerance = 1e-8)
})

test_that("omega_h = TRUE switches scale_reliability() to the 3-factor (hierarchical) fit", {
  skip_if_not_installed("psych")
  d <- make_bfi_subset()

  rel_h <- quietly(scale_reliability(d, agree_items, omega_h = TRUE))

  expect_equal(rel_h$omega$omega.tot, 0.8623313097, tolerance = 1e-8)
  # and it must actually differ from the default total-omega fit, or this
  # test couldn't tell the two branches apart
  rel_t <- quietly(scale_reliability(d, agree_items, omega_h = FALSE))
  expect_false(isTRUE(all.equal(rel_h$omega$omega.tot, rel_t$omega$omega.tot)))
})


# ---- scale_reliability(), grouped -------------------------------------------

test_that("scale_reliability() computes reliability separately per group", {
  skip_if_not_installed("psych")
  d <- make_bfi_subset()

  rel <- quietly(scale_reliability(d, agree_items, group = "gender", groups = c(1, 2)))

  expect_named(rel, c("1", "2"))
  expect_equal(unname(rel[["1"]]$uni$uni["u"]), 0.7454674903, tolerance = 1e-8)
  expect_equal(rel[["1"]]$omega$omega.tot,      0.7589231188, tolerance = 1e-8)
  expect_equal(unname(rel[["2"]]$uni$uni["u"]), 0.8741031338, tolerance = 1e-8)
  expect_equal(rel[["2"]]$omega$omega.tot,      0.7419443345, tolerance = 1e-8)
})

test_that("scale_reliability() errors when groups is supplied without a group name", {
  # NB: the validation only fires in this direction (groups given, group
  # missing) -- see the check at the top of scale_reliability(). Supplying
  # `group` without `groups` is not currently validated at all; that is a
  # separate, pre-existing gap, not the omega_h bug this file targets.
  d <- make_bfi_subset()
  expect_error(scale_reliability(d, agree_items, groups = c(1, 2)),
               "did not specify the group levels")
})


# ---- scale_reliabilities() forwards omega_h (regression for the fixed bug) --

test_that("scale_reliabilities() forwards omega_h rather than hardcoding FALSE", {
  skip_if_not_installed("psych")
  d <- make_bfi_subset()

  direct <- quietly(scale_reliability(d, agree_items, omega_h = TRUE))
  via_scales <- quietly(
    scale_reliabilities(d, list(agree = agree_items), omega_h = TRUE)
  )

  # forwarding omega_h = TRUE must reach the same 3-factor fit as calling
  # scale_reliability() directly with omega_h = TRUE ...
  expect_equal(via_scales$agree$omega$omega.tot, direct$omega$omega.tot,
               tolerance = 1e-8)
  expect_equal(via_scales$agree$omega$omega.tot, 0.8623313097, tolerance = 1e-8)

  # ... and, crucially, must NOT match what the old hardcoded omega_h = FALSE
  # would have produced -- this is exactly the value that a regression back
  # to the bug would silently return instead.
  expect_false(isTRUE(all.equal(via_scales$agree$omega$omega.tot, 0.7585117140)))
})

test_that("scale_reliabilities() still defaults omega_h to FALSE", {
  skip_if_not_installed("psych")
  d <- make_bfi_subset()

  direct <- quietly(scale_reliability(d, agree_items))
  via_scales <- quietly(scale_reliabilities(d, list(agree = agree_items)))

  expect_equal(via_scales$agree$omega$omega.tot, direct$omega$omega.tot,
               tolerance = 1e-8)
  expect_equal(via_scales$agree$omega$omega.tot, 0.7585117140, tolerance = 1e-8)
})

test_that("scale_reliabilities() applies the same omega_h to every scale", {
  skip_if_not_installed("psych")
  d <- make_bfi_subset()

  rels <- quietly(scale_reliabilities(
    d, list(agree = agree_items, consc = consc_items), omega_h = TRUE
  ))

  expect_named(rels, c("agree", "consc"))
  expect_equal(rels$agree$omega$omega.tot, 0.8623313097, tolerance = 1e-8)
  # consc's own hierarchical fit must differ from its total-omega fit too,
  # confirming omega_h reached the second scale as well, not just the first
  consc_total <- quietly(scale_reliability(d, consc_items, omega_h = FALSE))
  expect_false(isTRUE(all.equal(rels$consc$omega$omega.tot,
                                consc_total$omega$omega.tot)))
})


# ---- tidy_scale_reliability() ------------------------------------------------

test_that("tidy_scale_reliability() tidies an ungrouped result", {
  skip_if_not_installed("psych")
  d <- make_bfi_subset()
  rel <- quietly(scale_reliability(d, agree_items))

  tidy <- tidy_scale_reliability(rel)

  expect_named(tidy, c("group", "u", "alpha", "omega_tot"))
  expect_identical(tidy$group, "overall")
  expect_equal(tidy$u,         0.86, tolerance = 1e-8)
  expect_equal(tidy$alpha,     0.74, tolerance = 1e-8)
  expect_equal(tidy$omega_tot, 0.76, tolerance = 1e-8)
})

test_that("tidy_scale_reliability() tidies a grouped result and skips rounding when asked", {
  skip_if_not_installed("psych")
  d <- make_bfi_subset()
  rel <- quietly(scale_reliability(d, agree_items, group = "gender", groups = c(1, 2)))

  tidy <- tidy_scale_reliability(rel, rnd = FALSE)

  expect_identical(tidy$group, c("1", "2"))
  expect_equal(tidy$u[1], 0.7454674903, tolerance = 1e-8)
  expect_equal(tidy$u[2], 0.8741031338, tolerance = 1e-8)
})


# ---- tidy_scale_reliabilities() ----------------------------------------------

test_that("tidy_scale_reliabilities() stacks multiple scales into one long data frame", {
  skip_if_not_installed("psych")
  d <- make_bfi_subset()
  rels <- quietly(scale_reliabilities(d, list(agree = agree_items, consc = consc_items)))

  tidy <- tidy_scale_reliabilities(rels)

  expect_named(tidy, c("scale", "group", "u", "alpha", "omega_tot"))
  expect_identical(tidy$scale, c("agree", "consc"))
  expect_true(all(tidy$group == "overall"))
  expect_equal(tidy$omega_tot, c(0.76, 0.76), tolerance = 1e-8)
})


# ---- omega_specific() -------------------------------------------------------
#
# Needs a fitted lavaan bifactor CFA: one general factor loading on all 8
# items, plus two orthogonal specific factors (4 items each). Loadings and n
# are chosen so the CFA converges cleanly without identification warnings.

make_bifactor_data <- function(seed = 42, n = 500) {
  set.seed(seed)
  g  <- rnorm(n)
  s1 <- rnorm(n)
  s2 <- rnorm(n)

  mk <- function(load_g, load_s, s) {
    load_g * g + load_s * s + rnorm(n, sd = sqrt(1 - load_g^2 - load_s^2))
  }

  data.frame(
    i1 = mk(0.60, 0.50, s1), i2 = mk(0.65, 0.45, s1),
    i3 = mk(0.55, 0.50, s1), i4 = mk(0.60, 0.40, s1),
    i5 = mk(0.60, 0.50, s2), i6 = mk(0.65, 0.45, s2),
    i7 = mk(0.55, 0.50, s2), i8 = mk(0.60, 0.40, s2)
  )
}

bifactor_model <- '
  g  =~ i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8
  s1 =~ i1 + i2 + i3 + i4
  s2 =~ i5 + i6 + i7 + i8
'

fit_bifactor <- function() {
  d <- make_bifactor_data()
  lavaan::cfa(bifactor_model, data = d, orthogonal = TRUE, std.lv = TRUE)
}

test_that("omega_specific() returns one row per auto-detected specific factor", {
  skip_if_not_installed("lavaan")
  fit <- fit_bifactor()

  out <- omega_specific(fit, general = "g")

  expect_s3_class(out, "data.frame")
  expect_named(out, c("specific_factor", "n_items", "omega_specific"))
  expect_identical(out$specific_factor, c("s1", "s2"))
  expect_identical(out$n_items, c(4L, 4L))
  expect_true(all(out$omega_specific > 0 & out$omega_specific < 1))
})

test_that("omega_specific() matches a hand-computed value from standardizedSolution()", {
  skip_if_not_installed("lavaan")
  fit <- fit_bifactor()

  std <- lavaan::standardizedSolution(fit)
  loadings <- std[std$op == "=~", c("lhs", "rhs", "est.std")]

  lambda_g <- stats::setNames(loadings$est.std[loadings$lhs == "g"],
                               loadings$rhs[loadings$lhs == "g"])
  s1_rows <- loadings[loadings$lhs == "s1", ]
  lambda_s1 <- stats::setNames(s1_rows$est.std, s1_rows$rhs)
  lg <- lambda_g[names(lambda_s1)]
  theta <- 1 - lg^2 - lambda_s1^2
  expected_s1 <- sum(lambda_s1)^2 /
    (sum(lg)^2 + sum(lambda_s1)^2 + sum(theta))

  out <- omega_specific(fit, general = "g")
  expect_equal(out$omega_specific[out$specific_factor == "s1"], expected_s1,
               tolerance = 1e-8)
})

test_that("omega_specific() respects an explicit `specific` argument", {
  skip_if_not_installed("lavaan")
  fit <- fit_bifactor()

  out <- omega_specific(fit, general = "g", specific = "s2")
  expect_identical(out$specific_factor, "s2")
  expect_identical(nrow(out), 1L)
})

test_that("omega_specific() rejects bad input", {
  skip_if_not_installed("lavaan")
  fit <- fit_bifactor()

  expect_error(omega_specific("not a lavaan fit", general = "g"), "lavaan")
  expect_error(omega_specific(fit, general = c("g", "s1")), "single character string")
  expect_error(omega_specific(fit, general = "not_a_factor"), "not a latent factor")
  expect_error(omega_specific(fit, general = "g", specific = "not_a_factor"),
               "not found in `fit`")
  expect_error(omega_specific(fit, general = "g", specific = "g"),
               "cannot be its own specific factor")
  expect_error(omega_specific(fit, general = "g", specific = character(0)),
               "No specific factors found")
})


# ---- general-factor auto-detection -------------------------------------------
#
# .detect_general_factor() is tested directly on synthetic loading tables so
# the near-tie cases can be constructed exactly, without hunting for a
# bifactor model that happens to fit with a particular loading pattern.

mk_loadings <- function(...) {
  spec <- list(...)
  do.call(rbind, lapply(names(spec), function(f) {
    data.frame(lhs = f, rhs = spec[[f]], est.std = 0.6,
               stringsAsFactors = FALSE)
  }))
}

test_that(".detect_general_factor() picks the factor loading on the most items", {
  l <- mk_loadings(g = paste0("i", 1:6), s1 = paste0("i", 1:3),
                   s2 = paste0("i", 4:6))
  expect_identical(
    .detect_general_factor(l, c("g", "s1", "s2")), "g")
})

test_that(".detect_general_factor() is silent when the margin is clear", {
  l <- mk_loadings(g = paste0("i", 1:5), s1 = paste0("i", 1:3))
  expect_no_warning(.detect_general_factor(l, c("g", "s1")))
})

test_that(".detect_general_factor() warns on an exact tie", {
  # two equally broad factors: nothing distinguishes the general one
  l <- mk_loadings(g = paste0("i", 1:4), s1 = paste0("i", 1:4))
  expect_warning(.detect_general_factor(l, c("g", "s1")),
                 "too narrow to infer")
})

test_that(".detect_general_factor() warns when the runner-up is one item behind", {
  l <- mk_loadings(g = paste0("i", 1:4), s1 = paste0("i", 1:3))
  expect_warning(.detect_general_factor(l, c("g", "s1")), "Pass `general`")
})

test_that(".detect_general_factor() errors when there is only one factor", {
  l <- mk_loadings(g = paste0("i", 1:4))
  expect_error(.detect_general_factor(l, "g"), "Only one latent factor")
})

test_that("omega_specific() auto-detects the general factor when it is omitted", {
  skip_if_not_installed("lavaan")
  fit <- fit_bifactor()

  # g loads on 8 items, s1 and s2 on 4 each: unambiguous, so no warning
  expect_no_warning(auto <- omega_specific(fit))
  expect_identical(auto, omega_specific(fit, general = "g"))
})
