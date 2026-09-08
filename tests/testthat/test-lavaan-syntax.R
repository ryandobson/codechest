# Tests for the lavaan syntax toolkit: block parsers, the set_*() setters,
# and build_model() on top of them.
#
# The load-bearing tests are the end-to-end ones. A generated model string
# can look right and still be rejected by lavaan, so the important checks
# actually fit the generated syntax against HolzingerSwineford1939, which
# ships with lavaan and needs no data file of our own.
#
# set_variances() warns unconditionally by design (a documented inherited
# limitation), so calls that reach it are wrapped in suppressWarnings()
# except where the warning itself is what's being tested.


# ---- block parsers ----------------------------------------------------------

test_that("parse_single_block() returns one block covering the whole string", {
  blocks <- parse_single_block("f1 =~ x1 + x2 + x3")
  expect_length(blocks, 1L)
  expect_true(any(grepl("f1 =~", blocks[[1]]$lines)))
})

test_that("group_syntax() writes one group: header per group", {
  ms <- group_syntax("", c("a", "b"))
  expect_match(ms, "group:\\s*a")
  expect_match(ms, "group:\\s*b")
})

test_that("parse_group_blocks() returns one block per group", {
  ms <- group_syntax("", c("a", "b"))
  blocks <- parse_group_blocks(ms)

  expect_length(blocks, 2L)
  expect_identical(vapply(blocks, function(b) b$indices$group_name,
                          character(1)),
                   c("a", "b"))
  expect_identical(vapply(blocks, function(b) b$indices$block_id, integer(1)),
                   1:2)
})

test_that("parse_group_blocks() errors on a string with no group: lines", {
  expect_error(parse_group_blocks("f1 =~ x1 + x2"), "No 'group:' blocks")
})

test_that("labels lavaan cannot parse bare are quoted, and read back unquoted", {
  # lavaan rejects `group: Grant-White` but accepts it quoted; the round
  # trip has to survive that, or groups_to_fix stops matching
  ms <- group_syntax("", c("Pasteur", "Grant-White"))
  expect_match(ms, '"Grant-White"', fixed = TRUE)
  expect_false(grepl('"Pasteur"', ms, fixed = TRUE))

  names_back <- vapply(parse_group_blocks(ms),
                       function(b) b$indices$group_name, character(1))
  expect_identical(names_back, c("Pasteur", "Grant-White"))
})


# ---- setters ----------------------------------------------------------------

test_that("set_loadings() labels every loading under equal_all", {
  out <- set_loadings("f1 =~ x1 + x2 + x3", parse_single_block, "f1",
                      c("x1", "x2", "x3"))
  expect_match(out, "l1\\*x1")
  expect_match(out, "l2\\*x2")
  expect_match(out, "l3\\*x3")
})

test_that("set_loadings() leaves the named item free under equal_except", {
  out <- set_loadings("f1 =~ x1 + x2 + x3", parse_single_block, "f1",
                      c("x1", "x2", "x3"), mode = "equal_except",
                      free_items = "x2")
  expect_match(out, "l1\\*x1")
  expect_false(grepl("l2\\*x2", out))
})

test_that("set_loadings() rejects effects coding outside equal_all", {
  expect_error(
    set_loadings("f1 =~ x1 + x2", parse_single_block, "f1", c("x1", "x2"),
                 mode = "free_all", scale_id = "effects_coded"),
    "requires mode = 'equal_all'")
})

test_that("set_loadings() requires marker_items when scale_id is marker", {
  expect_error(
    set_loadings("f1 =~ x1 + x2", parse_single_block, "f1", c("x1", "x2"),
                 scale_id = "marker"),
    "no marker_items supplied")
})

test_that("set_intercepts() writes one labelled intercept per item", {
  out <- set_intercepts("f1 =~ x1 + x2", parse_single_block, "f1",
                        c("x1", "x2"))
  expect_match(out, "x1 ~ i1\\*1")
  expect_match(out, "x2 ~ i2\\*1")
})

test_that("set_residuals() writes one labelled residual per item", {
  out <- set_residuals("f1 =~ x1 + x2", parse_single_block, c("x1", "x2"))
  expect_match(out, "x1 ~~ r1\\*x1")
  expect_match(out, "x2 ~~ r2\\*x2")
})

test_that("set_item_covariances() errors on items outside the item set", {
  expect_error(
    set_item_covariances("f1 =~ x1 + x2", parse_single_block,
                         items = c("x1", "x2"), item_covars = c(x1 = "zz")),
    "not found in `items`")
})

test_that("set_item_covariances() writes the requested covariance", {
  out <- set_item_covariances("f1 =~ x1 + x2 + x3", parse_single_block,
                              items = c("x1", "x2", "x3"),
                              item_covars = c(x1 = "x2"))
  expect_match(out, "x1 ~~ .*x2")
})

test_that("set_factor_covariances() errors on a factor absent from the model", {
  expect_error(
    set_factor_covariances("f1 =~ x1 + x2", parse_single_block,
                           c("f1", "nope")),
    "not found in `model_string`")
})

test_that("set_variances() warns about its known removal limitation", {
  # the warning is deliberate and documented; assert it rather than lose it
  expect_warning(set_variances("f1 =~ x1 + x2", parse_single_block, "f1"),
                 "error in the removal of factor")
})

test_that("set_means() writes a latent mean line", {
  out <- set_means("f1 =~ x1 + x2", parse_single_block, "f1")
  expect_match(out, "f1 ~ 0\\*1")
})


# ---- build_model(), structure -----------------------------------------------

test_that("build_model() assembles every parameter type for one factor", {
  ms <- suppressWarnings(build_model(factors = list(f1 = c("x1", "x2", "x3"))))

  expect_match(ms, "f1 =~ l1\\*x1")
  expect_match(ms, "x1 ~ i1\\*1")
  expect_match(ms, "x1 ~~ r1\\*x1")
  expect_match(ms, "f1 ~~ 1\\*f1")
  expect_match(ms, "f1 ~ 0\\*1")
})

test_that("build_model() reuses labels across groups, which is the constraint", {
  ms <- suppressWarnings(build_model(
    factors = list(f1 = c("x1", "x2", "x3")),
    type    = "groups",
    groups  = c("a", "b")))

  # the same label appearing in both blocks is what equates the parameter
  expect_identical(length(gregexpr("l1\\*x1", ms)[[1]]), 2L)
  # first group identified, second free
  expect_match(ms, "f1 ~~ 1\\*f1")
  expect_match(ms, "f1 ~~ NA\\*f1")
})

test_that("build_model() errors when type is groups but groups is NULL", {
  expect_error(
    suppressWarnings(build_model(factors = list(f1 = c("x1", "x2")),
                                 type = "groups")),
    "'groups' was NULL")
})

test_that("build_model() rejects the unimplemented 'factors' type", {
  # accepted by match.arg but never wired up; must error rather than
  # silently produce a single-block model
  expect_error(
    suppressWarnings(build_model(factors = list(f1 = c("x1", "x2")),
                                 type = "factors")),
    "Unknown parser")
})

test_that("build_model() errors when marker identification lacks a marker item", {
  expect_error(
    suppressWarnings(build_model(factors = list(f1 = c("x1", "x2")),
                                 loadings_scale_id = "marker")),
    "no marker item was supplied")
})

test_that("build_model() gives each factor a distinct label prefix", {
  ms <- suppressWarnings(build_model(
    factors = list(f1 = c("x1", "x2", "x3"), f2 = c("y1", "y2", "y3"))))
  expect_match(ms, "f1_l1\\*x1")
  expect_match(ms, "f2_l1\\*y1")
})

test_that("build_model() writes a cross-loading item's residual only once", {
  # an item has one residual however many factors it loads on; writing it
  # twice produces a duplicate line lavaan rejects
  ms <- suppressWarnings(build_model(
    factors = list(f1 = c("x1", "x2", "x3"), f2 = c("x3", "y1", "y2"))))
  expect_identical(length(gregexpr("x3 ~~ [^\\n]*x3", ms)[[1]]), 1L)
})

test_that("build_model() adds factor covariances only with 2+ factors", {
  one <- suppressWarnings(build_model(factors = list(f1 = c("x1", "x2", "x3"))))
  two <- suppressWarnings(build_model(
    factors = list(f1 = c("x1", "x2", "x3"), f2 = c("y1", "y2", "y3"))))

  expect_false(grepl("phi", one))
  expect_match(two, "f1 ~~ .*f2")
})

test_that("build_model() writes the effects-coded sum constraint", {
  ms <- suppressWarnings(build_model(
    factors = list(f1 = c("x1", "x2", "x3")),
    loadings_scale_id = "effects_coded"))
  expect_match(ms, "==")
})

test_that("build_model(verbose = TRUE) reports per-factor detail without erroring", {
  # regression test: the verbose block used to reference factor_name and
  # items before the loop that assigns them, so it always errored
  expect_message(
    suppressWarnings(build_model(factors = list(f1 = c("x1", "x2", "x3")),
                                 verbose = TRUE)),
    "Latent factor: f1")
})


# ---- build_model(), does lavaan actually accept the syntax? ------------------

test_that("a generated single-group model fits in lavaan", {
  skip_if_not_installed("lavaan")

  ms <- suppressWarnings(build_model(
    factors = list(visual = c("x1", "x2", "x3"))))

  fit <- lavaan::cfa(ms, data = lavaan::HolzingerSwineford1939)
  expect_s4_class(fit, "lavaan")
  expect_true(lavaan::lavInspect(fit, "converged"))
})

test_that("a generated two-factor model fits and estimates the covariance", {
  skip_if_not_installed("lavaan")

  ms <- suppressWarnings(build_model(
    factors = list(visual  = c("x1", "x2", "x3"),
                   textual = c("x4", "x5", "x6"))))

  fit <- lavaan::cfa(ms, data = lavaan::HolzingerSwineford1939)
  expect_true(lavaan::lavInspect(fit, "converged"))

  pe <- lavaan::parameterEstimates(fit)
  expect_true(any(pe$lhs == "visual" & pe$rhs == "textual" & pe$op == "~~"))
})

test_that("a generated multi-group model fits with equality constraints", {
  skip_if_not_installed("lavaan")

  ms <- suppressWarnings(build_model(
    factors = list(visual = c("x1", "x2", "x3")),
    type    = "groups",
    groups  = c("Pasteur", "Grant-White")))

  fit <- lavaan::cfa(ms, data = lavaan::HolzingerSwineford1939,
                     group = "school")
  expect_true(lavaan::lavInspect(fit, "converged"))
  expect_identical(lavaan::lavInspect(fit, "ngroups"), 2L)

  # the shared labels should make the loadings equal across groups
  est <- lavaan::parameterEstimates(fit)
  l1 <- est$est[est$label == "l1" & est$op == "=~"]
  expect_equal(length(unique(round(l1, 10))), 1L)
})

test_that("freeing a loading actually relaxes the cross-group constraint", {
  skip_if_not_installed("lavaan")

  constrained <- suppressWarnings(build_model(
    factors = list(visual = c("x1", "x2", "x3")),
    type = "groups", groups = c("Pasteur", "Grant-White")))

  partial <- suppressWarnings(build_model(
    factors = list(visual = c("x1", "x2", "x3")),
    type = "groups", groups = c("Pasteur", "Grant-White"),
    loadings_mode = "equal_except", free_loadings = "x2"))

  f_con <- lavaan::cfa(constrained, data = lavaan::HolzingerSwineford1939,
                       group = "school")
  f_par <- lavaan::cfa(partial, data = lavaan::HolzingerSwineford1939,
                       group = "school")

  # freeing a parameter costs a degree of freedom and cannot worsen fit
  expect_lt(lavaan::fitMeasures(f_par, "df"),
            lavaan::fitMeasures(f_con, "df"))
  expect_lte(lavaan::fitMeasures(f_par, "chisq"),
             lavaan::fitMeasures(f_con, "chisq") + 1e-6)
})
