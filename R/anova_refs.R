# ============================================================
# anova_refs.R
#
# Layer 4: the reference registry.
#
# One entry per statistical concept. Tests, recommendations, print
# methods, and teaching material all pull citations and equations from
# here, so a reference is written once and correct everywhere.
#
# NUMBERING
#   MDK numbers equations sequentially WITHIN a chapter and cites them
#   as "Equation 47", with no chapter prefix. So `mdk_ch` and `mdk_eq`
#   must always be read together.
#
# HONESTY RULE
#   `verified = TRUE` means the chapter and equation were read out of
#   the book, not inferred. Entries that have not been checked keep
#   `mdk_eq = NA`. Do not guess one: a wrong equation number is worse
#   than a missing one, because a reader cannot tell it is wrong.
#   anova_refs_todo() lists what is still outstanding.
# ============================================================


#' The MDK textbook citation, in one place.
#' @keywords internal
.mdk_citation <- paste(
  "Maxwell, S. E., Delaney, H. D., & Kelly, K. (2017).",
  "Designing experiments and analyzing data: A model comparison",
  "perspective (3rd ed.). New York: Taylor & Francis."
)

#' Build one registry entry.
#' @keywords internal
.ref <- function(concept, label, mdk_ch = NA_integer_, mdk_eq = NA_character_,
                 equation = NA_character_, gloss = NA_character_,
                 also = character(0), verified = FALSE) {
  list(concept = concept, label = label, mdk_ch = mdk_ch, mdk_eq = mdk_eq,
       equation = equation, gloss = gloss, also = also, verified = verified)
}

# ---- the registry ----------------------------------------------------------

.anova_refs <- list(

  full_model = .ref(
    "full_model", "Full model, one-way design",
    mdk_ch = 3, mdk_eq = "47", verified = TRUE,
    equation = "Y_ij = mu_j + eps_ij",
    gloss = paste(
      "One parameter per group: each observation is predicted by its own",
      "group's mean. The restricted model (Equation 48) collapses those to",
      "a single mu for everyone, and the F test asks whether that",
      "simplification costs too much.")
  ),

  restricted_model = .ref(
    "restricted_model", "Restricted model, one-way design",
    mdk_ch = 3, mdk_eq = "48", verified = TRUE,
    equation = "Y_ij = mu + eps_ij",
    gloss = "A single mean for everybody, ignoring group membership."
  ),

  model_comparison = .ref(
    "model_comparison", "F as a comparison of two models",
    mdk_ch = 3, mdk_eq = "75", verified = TRUE,
    equation = "F = [(E_R - E_F) / E_F] * [df_F / (df_R - df_F)]",
    gloss = paste(
      "The proportional increase in error from simplifying the model,",
      "scaled by the degrees of freedom given up. Algebraically the",
      "familiar F, but written so the logic is visible: a test statistic",
      "is an index of effect size times an index of study size.")
  ),

  sum_squares_between = .ref(
    "sum_squares_between", "Reduction in error from the group means",
    mdk_ch = 3, mdk_eq = "51", verified = TRUE,
    equation = "E_R - E_F = sum_j n_j (Ybar_j - Ybar)^2",
    gloss = paste(
      "How much better the full model does, in squared error. This is",
      "SS_Between in traditional notation.")
  ),

  sample_variance = .ref(
    "sample_variance", "Unbiased sample variance",
    mdk_ch = 3, mdk_eq = "11", verified = TRUE,
    equation = "s^2 = sum_i (Y_i - Ybar)^2 / (n - 1)",
    gloss = "The building block E_F is assembled from."
  ),

  ms_within = .ref(
    "ms_within", "Pooled within-group variance",
    mdk_ch = 3, mdk_eq = "56", verified = TRUE,
    equation = "E_F / df_F = sum_j s_j^2 / a     (equal n)",
    gloss = paste(
      "The ANOVA's single estimate of the population error variance,",
      "pooled across groups. In the equal-n case it is just the average of",
      "the group variances. This is what homogeneity of variance licenses,",
      "and what MS_W means in the output of aov().")
  ),

  df_full = .ref(
    "df_full", "Degrees of freedom of the full model",
    mdk_ch = 3,
    equation = "df_F = N - a",
    gloss = paste(
      "Total observations minus one mean estimated per group. MDK develop",
      "this in the text rather than as a numbered equation.")
  ),

  se_mean = .ref(
    "se_mean", "Standard error of a group mean",
    mdk_ch = 3,
    equation = "SE(Ybar_j) = sqrt(MS_W / n_j)",
    gloss = paste(
      "The model-based standard error. Uses the pooled variance rather",
      "than the group's own, so it is consistent with the F test reported",
      "alongside it. Error bars built this way match the model; error bars",
      "built from each group's own SD do not.")
  ),

  normality = .ref(
    "normality", "Normality of residuals",
    mdk_ch = 3, verified = TRUE,
    gloss = paste(
      "The assumption concerns the residuals, not the raw scores. Raw",
      "scores pool groups with different means and can look non-normal",
      "when every group is perfectly normal. MDK treat this among the",
      "assumptions of the one-way model in Chapter 3; the F test is robust",
      "to moderate departures, particularly with equal n."),
    also = c("Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). Biometrika, 52(3-4), 591-611.")
  ),

  homogeneity = .ref(
    "homogeneity", "Homogeneity of variance",
    mdk_ch = 3, verified = TRUE,
    gloss = paste(
      "Equal population variances across groups. This is what licenses",
      "pooling into a single MS_W (Equation 56). MDK discuss Levene's test",
      "in Chapter 3 and describe it exactly as implemented here: an ANOVA",
      "on the absolute deviations of scores from their group's centre.",
      "Violations matter most when group sizes are also unequal."),
    also = c(
      "Levene, H. (1960). Robust tests for equality of variances. In I. Olkin (Ed.), Contributions to Probability and Statistics (pp. 278-292). Stanford University Press.",
      "Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of variances. Journal of the American Statistical Association, 69(346), 364-367.")
  ),

  independence = .ref(
    "independence", "Independence of observations",
    mdk_ch = 3,
    gloss = paste(
      "Not testable from the data. It is a property of how the study was",
      "run. Violated by clustering, repeated measurement, shared testing",
      "sessions, or any route by which one observation could influence",
      "another.")
  ),

  welch_anova = .ref(
    "welch_anova", "Welch's W and the Brown-Forsythe F*",
    mdk_ch = 3, verified = TRUE,
    gloss = paste(
      "Robust alternatives that do not assume equal variances; they",
      "estimate the within-group variance differently and end up with",
      "fractional denominator df. MDK cover these in the Chapter 3",
      "extension on DesigningExperiments.com rather than the main text,",
      "and are specific about when they help: preferable to the standard F",
      "when sample sizes are unequal AND variances are heterogeneous, and",
      "preferable ONLY when the population variances really are unequal.",
      "In R: oneway.test(y ~ group, var.equal = FALSE)."),
    also = c(
      "Welch, B. L. (1951). On the comparison of several mean values: An alternative approach. Biometrika, 38(3-4), 330-336.",
      "Delacre, M., Lakens, D., & Leys, C. (2017). Why psychologists should by default use Welch's t-test instead of Student's t-test. International Review of Social Psychology, 30(1), 92-101.")
  ),

  r_squared = .ref(
    "r_squared", "R squared (eta squared)",
    mdk_ch = 3, mdk_eq = "94", verified = TRUE,
    equation = "R^2 = (E_R - E_F) / E_R",
    gloss = paste(
      "The proportional reduction in error, and the proportion of",
      "variability in the sample accounted for by the treatment. NOTE ON",
      "NAMING: MDK call this R^2. Most of psychology calls the same",
      "quantity eta squared in the ANOVA context. They are identical in a",
      "one-way design. Biased upward as an estimate of the population",
      "value."),
    also = c("Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: A practical primer for t-tests and ANOVAs. Frontiers in Psychology, 4, 863.")
  ),

  omega_squared_pop = .ref(
    "omega_squared_pop", "Omega squared, the population quantity",
    mdk_ch = 3, mdk_eq = "95", verified = TRUE,
    gloss = paste(
      "The proportion of total population variance on the outcome that is",
      "due to variation in the population treatment means. This is the",
      "thing you actually want; R^2 is a biased estimate of it.")
  ),

  omega_squared = .ref(
    "omega_squared", "Omega hat squared, the bias-corrected estimate",
    mdk_ch = 3, mdk_eq = "96", verified = TRUE,
    equation = "omegahat^2 = (SS_effect - df_effect * MS_W) / (SS_total + MS_W)",
    gloss = paste(
      "Corrects the positive bias in R^2. Report this in preference to",
      "R^2 / eta squared. MDK note it can come out negative, in which case",
      "the estimated population proportion is set to zero. See also the",
      "adjusted R^2 of Equation 99, which Maxwell et al. (1981) showed is",
      "typically within .02 of omega hat squared."),
    also = c("Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: A practical primer for t-tests and ANOVAs. Frontiers in Psychology, 4, 863.")
  ),

  cohens_d = .ref(
    "cohens_d", "Standardized mean difference",
    mdk_ch = 3, mdk_eq = "81", verified = TRUE,
    equation = "d = (Ybar_1 - Ybar_2) / s_p",
    gloss = paste(
      "Difference between two means in pooled standard deviation units.",
      "Lakens (2013) is the practical guide to which variant to report: d_s",
      "for between-subjects designs, d_av or d_z for within-subjects, and",
      "they are not interchangeable, so state which one you used."),
    also = c("Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: A practical primer for t-tests and ANOVAs. Frontiers in Psychology, 4, 863.")
  ),

  hedges_g = .ref(
    "hedges_g", "Hedges's g, the small-sample correction to d",
    verified = TRUE,
    equation = "g = d * (1 - 3 / (4 * (n1 + n2) - 9))",
    gloss = paste(
      "Cohen's d is biased upward in small samples. Hedges's g applies a",
      "multiplicative correction that shrinks it. The correction is",
      "negligible once n is large, so applying it always costs nothing and",
      "Lakens recommends defaulting to it."),
    also = c("Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: A practical primer for t-tests and ANOVAs. Frontiers in Psychology, 4, 863.")
  ),

  generalized_eta_squared = .ref(
    "generalized_eta_squared", "Generalized eta squared",
    verified = TRUE,
    gloss = paste(
      "Partial eta squared is not comparable between between-subjects and",
      "within-subjects designs, because what sits in the denominator",
      "changes with the design. Generalized eta squared is constructed to",
      "be comparable across designs, which is what makes it the right",
      "choice for meta-analysis and for any table that mixes designs. This",
      "becomes the relevant measure once the within-subjects engine lands."),
    also = c("Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: A practical primer for t-tests and ANOVAs. Frontiers in Psychology, 4, 863.",
             "Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared statistics: Measures of effect size for some common research designs. Psychological Methods, 8(4), 434-447.")
  ),


  # ---- robust methods (Field & Wilcox, 2017) ----

  robust_sensitivity = .ref(
    "robust_sensitivity", "Sensitivity analysis against a robust model",
    verified = TRUE,
    gloss = paste(
      "Field and Wilcox's central recommendation: fit the conventional",
      "model AND a robust counterpart, and compare. Where the two agree,",
      "report either. Where they deviate substantially, report the robust",
      "one unless there is an evidence-based case that the assumptions",
      "were met. Their stronger claim is that this comparison is the ONLY",
      "known way to judge whether a conventional method gave a reasonable",
      "answer, which is what anova_multi() automates."),
    also = c("Field, A. P., & Wilcox, R. R. (2017). Robust statistical methods: A primer for clinical psychology and experimental psychopathology researchers. Behaviour Research and Therapy, 98, 19-38.")
  ),

  normality_prevalence = .ref(
    "normality_prevalence", "How often psychological data are actually normal",
    verified = TRUE,
    gloss = paste(
      "Field and Wilcox review 440 real distributions: taking symmetry and",
      "tail weight together, 6.8% approximated normality, and up to two",
      "thirds had heavy tails. Their framing is that non-normality should",
      "be the default expectation rather than the exception, and that",
      "heavy tails matter more than skew because of the effect on power",
      "and on effect size estimates."),
    also = c("Field, A. P., & Wilcox, R. R. (2017). Robust statistical methods: A primer for clinical psychology and experimental psychopathology researchers. Behaviour Research and Therapy, 98, 19-38.")
  ),

  transformation = .ref(
    "transformation", "Transforming the outcome",
    verified = TRUE,
    gloss = paste(
      "A weaker answer to non-normality than it looks. Field and Wilcox",
      "give six reasons: the gain in validity of probability statements is",
      "low; transforming changes the hypothesis (comparing means of logs",
      "compares geometric means); it transforms the construct as well as",
      "the numbers; you must be able to argue the wrong transformation is",
      "less costly than none; heavy tails matter more than skew and a",
      "transformation has to fix tails without worsening them; and",
      "distributions typically stay skewed afterwards anyway. Include one",
      "in a multiverse as a decision to test, not as a fix to apply."),
    also = c("Field, A. P., & Wilcox, R. R. (2017). Robust statistical methods: A primer for clinical psychology and experimental psychopathology researchers. Behaviour Research and Therapy, 98, 19-38.")
  ),

  outliers = .ref(
    "outliers", "Outlier handling",
    verified = TRUE,
    gloss = paste(
      "Standard-deviation-based trims (the reaction-time convention of",
      "cutting at 2.5 SD) are flawed, because the mean and especially the",
      "SD are themselves pulled by the outliers, so outliers get masked.",
      "Manual inspection and removal is worse: whatever you fit afterwards",
      "with OLS has incorrect standard errors, and the confidence",
      "intervals are inaccurate at any sample size. Percentage-based",
      "trimming with methods built for it is the defensible route."),
    also = c("Field, A. P., & Wilcox, R. R. (2017). Robust statistical methods: A primer for clinical psychology and experimental psychopathology researchers. Behaviour Research and Therapy, 98, 19-38.")
  ),

  trimmed_means = .ref(
    "trimmed_means", "Trimmed means and M-estimators",
    verified = TRUE,
    gloss = paste(
      "Estimators based on percentage trims perform well across a range of",
      "situations; a 20% trim is the usual recommendation. M-estimators",
      "decide empirically whether a score is an outlier and down-weight",
      "rather than discard it, avoiding over- or under-trimming. Critically",
      "you cannot trim and then apply OLS; use methods built for trimmed",
      "estimation. In R: WRS2::t1way() for a between-subjects trimmed",
      "means ANOVA, WRS2::rmanova() for within-subjects."),
    also = c("Field, A. P., & Wilcox, R. R. (2017). Robust statistical methods: A primer for clinical psychology and experimental psychopathology researchers. Behaviour Research and Therapy, 98, 19-38.",
             "Mair, P., & Wilcox, R. (2020). Robust statistical methods in R using the WRS2 package. Behavior Research Methods, 52, 464-488.",
             "Wilcox, R. R. (2017). Introduction to robust estimation and hypothesis testing (4th ed.). Academic Press.")
  ),

  bootstrap_ci = .ref(
    "bootstrap_ci", "Bootstrap standard errors and confidence intervals",
    verified = TRUE,
    gloss = paste(
      "Resample the data, refit, and take the middle 95% of the estimates",
      "as a percentile confidence interval; the BCa variant corrects for",
      "skew. Bootstrap standard errors, test statistics and intervals are",
      "robust to the violations discussed here, and Field and Wilcox note",
      "that even when no robust test exists for a design, you can",
      "bootstrap almost any model."),
    also = c("Field, A. P., & Wilcox, R. R. (2017). Robust statistical methods: A primer for clinical psychology and experimental psychopathology researchers. Behaviour Research and Therapy, 98, 19-38.")
  ),

  hc_standard_errors = .ref(
    "hc_standard_errors", "Heteroscedasticity-consistent standard errors",
    verified = TRUE,
    gloss = paste(
      "The Huber-White correction adjusts the standard errors for unequal",
      "residual variance without changing the parameter estimates. Under",
      "heteroscedasticity the usual SE formula is simply the wrong formula,",
      "so t, p, and the confidence interval are all biased; Wilcox notes",
      "intervals can be extremely inaccurate."),
    also = c("Field, A. P., & Wilcox, R. R. (2017). Robust statistical methods: A primer for clinical psychology and experimental psychopathology researchers. Behaviour Research and Therapy, 98, 19-38.")
  ),

  # ---- concepts for designs not yet implemented ----
  # Chapters are MDK's coverage of the topic; equation numbers pending.

  sphericity = .ref(
    "sphericity", "Sphericity",
    mdk_ch = 11,
    gloss = paste(
      "In within-subjects designs, the variances of all pairwise",
      "difference scores must be equal. Violation inflates the Type I",
      "error rate of the univariate F test."),
    also = c("Mauchly, J. W. (1940). Significance test for sphericity of a normal n-variate distribution. Annals of Mathematical Statistics, 11(2), 204-209.")
  ),

  sphericity_correction = .ref(
    "sphericity_correction", "Greenhouse-Geisser and Huynh-Feldt corrections",
    mdk_ch = 11,
    gloss = paste(
      "Multiply both degrees of freedom by an estimate of epsilon, the",
      "departure from sphericity. Greenhouse-Geisser is conservative;",
      "Huynh-Feldt is less so and generally preferred when epsilon is",
      "above about .75."),
    also = c(
      "Greenhouse, S. W., & Geisser, S. (1959). On methods in the analysis of profile data. Psychometrika, 24(2), 95-112.",
      "Huynh, H., & Feldt, L. S. (1976). Estimation of the Box correction for degrees of freedom from sample data in randomized block and split-plot designs. Journal of Educational Statistics, 1(1), 69-82.")
  ),

  homogeneity_covariance = .ref(
    "homogeneity_covariance", "Homogeneity of covariance matrices",
    mdk_ch = 13,
    gloss = paste(
      "In mixed designs, the between-subjects groups must share a common",
      "covariance structure across the within-subjects levels."),
    also = c("Box, G. E. P. (1954). Some theorems on quadratic forms applied in the study of analysis of variance problems, II. Annals of Mathematical Statistics, 25(3), 484-498.")
  ),

  multiple_comparisons = .ref(
    "multiple_comparisons", "Type I error inflation across comparisons",
    mdk_ch = 5,
    gloss = paste(
      "Each additional test carries its own chance of a false positive, so",
      "the familywise error rate exceeds the per-test alpha. Post hoc",
      "procedures exist to control it. MDK devote Chapter 4 to individual",
      "comparisons and Chapter 5 to testing several contrasts.")
  )
)


# ---- accessors -------------------------------------------------------------

#' Look up a statistical concept: citation, equation, and plain-English gloss
#'
#' Every assumption check and recommendation in the `anova_*` family cites a
#' concept key. This retrieves the full entry, so you can read the reasoning or
#' pull the equation while writing up.
#'
#' @param concept Character key, e.g. `"homogeneity"`. Omit to list all keys.
#' @param quiet If `TRUE`, return the entry without printing.
#'
#' @return Invisibly, the registry entry (a list). Called for its printed
#'   output by default.
#'
#' @examples
#' anova_ref()                 # list every concept
#' anova_ref("homogeneity")
#' anova_ref("omega_squared")
#'
#' @seealso [anova_refs_todo()] for entries still needing verification,
#'   [anova_textbook()] for the full citation.
#' @keywords documentation
#' @export
anova_ref <- function(concept = NULL, quiet = FALSE) {

  if (is.null(concept)) {
    cat("Concepts in the ANOVA reference registry:\n\n")
    for (k in names(.anova_refs)) {
      e <- .anova_refs[[k]]
      cat(sprintf("  %-22s %-46s %s\n", k, e$label,
                  if (!is.na(e$mdk_eq)) paste0("MDK ch.", e$mdk_ch, " eq.", e$mdk_eq)
                  else if (!is.na(e$mdk_ch)) paste0("MDK ch.", e$mdk_ch) else ""))
    }
    cat("\nanova_ref(\"<concept>\") for the full entry.\n")
    return(invisible(names(.anova_refs)))
  }

  if (!concept %in% names(.anova_refs)) {
    stop("Unknown concept: ", concept,
         "\n  anova_ref() lists the valid keys.", call. = FALSE)
  }
  e <- .anova_refs[[concept]]
  if (!quiet) print_anova_ref(e)
  invisible(e)
}

#' @keywords internal
print_anova_ref <- function(e) {
  cat("\n", e$label, "\n", sep = "")
  cat(strrep("-", nchar(e$label)), "\n", sep = "")

  if (!is.na(e$equation)) cat("\n  ", e$equation, "\n", sep = "")
  if (!is.na(e$gloss)) {
    cat("\n")
    for (ln in strwrap(e$gloss, width = 70)) cat("  ", ln, "\n", sep = "")
  }

  cat("\n  MDK: ",
      if (is.na(e$mdk_ch)) "chapter not recorded" else paste0("ch. ", e$mdk_ch),
      if (!is.na(e$mdk_eq)) paste0(", eq. ", e$mdk_eq) else "",
      if (!e$verified) "   [not yet verified against the book]" else "",
      "\n", sep = "")

  if (length(e$also)) {
    cat("\n  Also:\n")
    for (a in e$also) {
      for (ln in strwrap(a, width = 66)) cat("    ", ln, "\n", sep = "")
    }
  }
  cat("\n")
  invisible(e)
}

#' Which reference entries still need checking against the book
#'
#' Equation numbers are left `NA` until confirmed against a copy of MDK, on the
#' principle that a wrong equation number is worse than a missing one. This
#' lists what is outstanding.
#'
#' @return Invisibly, a data.frame of unverified entries.
#'
#' @examples
#' anova_refs_todo()
#'
#' @keywords documentation
#' @export
anova_refs_todo <- function() {

  rows <- lapply(.anova_refs, function(e) {
    data.frame(concept = e$concept, chapter = e$mdk_ch, equation = e$mdk_eq,
               verified = e$verified, stringsAsFactors = FALSE)
  })
  df   <- do.call(rbind, rows)
  todo <- df[!df$verified, , drop = FALSE]

  cat("MDK reference entries not yet verified against the book:\n\n")
  if (!nrow(todo)) {
    cat("  none, everything is confirmed.\n")
  } else {
    print(todo, row.names = FALSE)
    cat("\n", nrow(todo), " of ", nrow(df), " entries outstanding.\n", sep = "")
    cat("Chapters are topic-to-chapter assignments and are probably right;\n")
    cat("equation numbers are deliberately blank rather than guessed.\n")
    cat("Fill them in by editing .anova_refs in R/anova_refs.R.\n")
  }
  invisible(todo)
}

#' The MDK textbook citation
#'
#' @return The full reference, as a character string.
#' @examples
#' anova_textbook()
#' @keywords documentation
#' @export
anova_textbook <- function() .mdk_citation
