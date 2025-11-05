

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
#' @importFrom lavaan cfa
#' @importFrom stats anova
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


#' Run Multiple Measurement Invariance Tests
#'
#' This function tests measurement invariance for multiple models across multiple grouping variables.
#' It applies the `test_measurement_invariance` function to each combination of model and grouping variable
#' and returns a structured list of results.
#'
#' @param models A named list of models to test, where the names represent the model identifiers,
#'   and the values are the model strings in `lavaan` syntax.
#' @param data A data frame containing the observed variables used in the models.
#' @param group_vars A character vector specifying the grouping variables to test invariance against.
#' @param verbose Whether or not to print output to track the progress of the invariance
#' testing for each grouping variable. Default = 'TRUE'.
#'
#' @return A list where each element corresponds to a specific model and grouping variable combination.
#'   Each element contains the results returned by `test_measurement_invariance`, including:
#'   \describe{
#'     \item{configural}{The configural invariance model fit.}
#'     \item{metric}{The metric invariance model fit.}
#'     \item{scalar}{The scalar invariance model fit.}
#'     \item{strict}{The strict invariance model fit.}
#'     \item{comparisons}{The comparison of fit statistics across invariance levels.}
#'   }
#'
#' @details
#' This function is designed for scenarios where multiple measurement models need to be tested for invariance
#' across multiple grouping variables. It automates the process by iterating through all combinations of
#' models and grouping variables, running `test_measurement_invariance` for each combination, and storing
#' the results in a structured format.
#'
#' The function prints basic progress information to the console, including the current model and grouping
#' variable being processed, as well as the fit comparisons for each invariance test.
#'

#' @seealso \code{\link{test_measurement_invariance}}
#' @importFrom lavaan cfa
#' @importFrom stats anova
#' @export

run_multiple_invariance <- function(models, data, group_vars, verbose = TRUE) {

  results <- list()

  for (group_var in group_vars) {
    for (model_name in names(models)) {
      model <- models[[model_name]]
      # Run measurement invariance
      mi_result <- test_measurement_invariance(model, data, group_var)
      # Store results in a structured way
      results[[paste0(model_name, "_", group_var)]] <- mi_result

    if(verbose == TRUE) {
      # Print basic output for monitoring progress
      cat("\nModel:", model_name, "Group:", group_var, "\n")
      print(mi_result$comparisons)
    }
    }#end of inner for loop
  }#end of outer for loop

  return(results)
}



