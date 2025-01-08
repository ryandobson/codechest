
utils::globalVariables(c("anova", "cfa"))


#' Test Measurement Invariance Across Groups
#'
#' This function tests measurement invariance across groups by sequentially fitting
#' models with increasing constraints: configural, metric, scalar, and strict invariance.
#' It compares these models to assess the degree to which measurement properties
#' are equivalent across groups.
#'
#' @param model A character string specifying the measurement model to be tested.
#'   The model should follow the syntax of the `lavaan` package.
#' @param data A data frame containing the observed variables used in the model.
#' @param group A character string specifying the grouping variable in the `data`
#'   that defines the groups for invariance testing.
#'
#' @return A list containing:
#' \describe{
#'   \item{configural}{The fitted configural invariance model.}
#'   \item{metric}{The fitted metric invariance model.}
#'   \item{scalar}{The fitted scalar invariance model.}
#'   \item{strict}{The fitted strict invariance model.}
#'   \item{comparisons}{An `anova` table comparing the four models.}
#' }
#'
#' @details
#' The function sequentially imposes constraints to test different levels of measurement
#' invariance:
#' \itemize{
#'   \item \strong{Configural invariance:} Tests whether the overall factor structure
#'     (pattern of factor loadings) is the same across groups.
#'   \item \strong{Metric invariance:} Adds equality constraints to the factor loadings,
#'     testing whether groups respond to the constructs in a similar way.
#'   \item \strong{Scalar invariance:} Adds equality constraints to the intercepts,
#'     testing whether the item means are comparable across groups.
#'   \item \strong{Strict invariance:} Adds equality constraints to the residual variances,
#'     testing whether the item residuals are equivalent across groups.
#' }
#'
#' The `anova` table compares the models to determine if each level of invariance
#' is supported. A non-significant difference in model fit suggests invariance holds.
#'
#' @importFrom lavaan cfa anova
#' @export


test_measurement_invariance <- function(model, data, group) {
  # Configural Invariance
  fit_configural <- cfa(model, data = data, group = group, meanstructure = TRUE)

  # Metric Invariance
  fit_metric <- cfa(model, data = data, group = group, group.equal = "loadings", meanstructure = TRUE)

  # Scalar Invariance
  fit_scalar <- cfa(model, data = data, group = group, group.equal = c("loadings", "intercepts"), meanstructure = TRUE)

  # Strict Invariance
  fit_strict <- cfa(model, data = data, group = group, group.equal = c("loadings", "intercepts", "residuals"), meanstructure = TRUE)

  # Compare models
  comparisons <- anova(fit_configural, fit_metric, fit_scalar, fit_strict)

  # Return results
  list(
    configural = fit_configural,
    metric = fit_metric,
    scalar = fit_scalar,
    strict = fit_strict,
    comparisons = comparisons
  )
}
