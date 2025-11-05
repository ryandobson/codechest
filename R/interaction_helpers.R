#' @importFrom emmeans emtrends
NULL

#' @title Extract and Tidy a Simple Slope from a Mixed-Effects Model
#'
#' @description
#' Computes and tidies estimated marginal trends (simple slopes) for a given
#' focal variable across levels of a moderator using [emmeans::emtrends()].
#' Returns a clean, ready-to-plot data frame including slope estimates,
#' standard errors, degrees of freedom, confidence intervals, and p-values.
#'
#' @details
#' This function serves as a convenient wrapper around [emmeans::emtrends()]
#' that standardizes output into a tidy data frame for plotting and downstream
#' analysis. It also includes robust error handling for missing variables and
#' failed trend estimations.
#'
#' The moderator variable is specified via `specs`, and the focal variable for
#' which simple slopes are estimated is specified via `var`.
#' Moderator values at which slopes are estimated can be adjusted with
#' `probe_at` (defaults to ±1 SD and mean: `c(-1, 0, 1)`).
#'
#' @param model A fitted mixed-effects model object, typically from
#'   [lmerTest::lmer()].
#' @param specs Character string naming the moderator variable (passed to
#'   [emmeans::emtrends()] via the `specs` argument).
#' @param var Character string naming the focal predictor variable whose slope
#'   will be estimated.
#' @param probe_at Numeric vector giving the moderator values at which to probe
#'   simple slopes. Defaults to `c(-1, 0, 1)`.
#' @param mod_name Optional character string identifying the model name for
#'   labeling and tracking in combined outputs. Defaults to the object name of
#'   the model as given in the call.
#'
#' @return
#' A data frame with the following columns:
#' \describe{
#'   \item{model}{Model name or identifier.}
#'   \item{variable}{The focal variable whose slope was estimated.}
#'   \item{moderator_value}{The moderator values at which slopes were computed.}
#'   \item{slope}{Estimated simple slope of `var` at each moderator value.}
#'   \item{SE, df}{Standard error and degrees of freedom.}
#'   \item{lower_ci, upper_ci}{Confidence interval bounds.}
#'   \item{p.value}{Associated p-value for the slope.}
#' }
#'
#' @examples
#' \dontrun{
#' library(emmeans)
#' library(lme4)
#' m <- lmerTest::lmer(Reaction ~ Days * Subject + (Days | Subject), data = sleepstudy)
#'
#' tidy_emtrend(m, specs = "Subject", var = "Days", probe_at = c(-1, 0, 1))
#' }
#'
#' @seealso [emmeans::emtrends()], [tidy_emtrends()]
#'
#' @importFrom emmeans emtrends
#' @export
tidy_emtrend <- function(model,
                         specs,
                         var,
                         probe_at = c(-1, 0, 1),
                         mod_name = deparse(substitute(model))) {


  # Skip if variable not in model frame
  if (!(var %in% names(model@frame))) {
    message(paste("Skipping", var, "in", mod_name, "- variable not found in model frame."))
    return(NULL)
  }

  # Try to run emtrends()
  em_df <- tryCatch(
    as.data.frame(
      emtrends(
        model,
        specs = specs,
        var = var,
        at = setNames(list(probe_at), specs),
        pbkrtest.limit = 12487,
        lmerTest.limit = 12487,
        infer = TRUE
      )
    ),
    error = function(e) {
      message(paste("emtrends() failed for", var, "in", mod_name, ":", e$message))
      return(NULL)
    }
  )


  # Extract the slope column name (e.g., "Zprc_stirn_ww.trend")
  slope_col <- paste0(var, ".trend")
  lower_ci <- colnames(em_df[grepl("asymp.LCL|lower.CL", names(em_df))])
  upper_ci <- colnames(em_df[grepl("asymp.UCL|upper.CL", names(em_df))])

  # Build a compact data.frame for plotting
  out <- data.frame(
    model = rep(mod_name, nrow(em_df)),
    variable = rep(var, nrow(em_df)),
    moderator_value = em_df[[specs]],
    slope = em_df[[slope_col]],
    SE = em_df$SE,
    df = em_df$df,
    lower_ci = em_df[[lower_ci]],
    upper_ci = em_df[[upper_ci]],
    p.value = em_df$p.value,
    stringsAsFactors = FALSE
  )

  return(out)
}




#' @title Batch Extraction of Simple Slopes Across Multiple Models and Variables
#'
#' @description
#' Applies [tidy_emtrend()] iteratively across a list of models and focal
#' variables, producing a combined tidy data frame of all estimated simple slopes.
#' This allows for automated extraction of interaction probes across multiple
#' models in a standardized format.
#'
#' @details
#' For each model in `model_list`, the function retrieves the model object from
#' the element specified by `model_path`, then computes simple slopes for each
#' focal variable in `vars` using [tidy_emtrend()].
#' Results from all models and variables are combined into a single tidy data
#' frame suitable for plotting or meta-analytic summary.
#'
#' @param model_list A named list of models (or model containers), each containing
#'   a fitted mixed-effects model object under the element specified by `model_path`.
#' @param specs Character string naming the moderator variable (passed to
#'   [tidy_emtrend()]).
#' @param vars Character vector of focal predictor variable names whose simple
#'   slopes should be estimated.
#' @param model_path Character string indicating where to find the model object
#'   within each list element (e.g., `"model"` or `"post_mlm$model"`).
#'
#' @return
#' A tidy data frame combining simple slope estimates across all specified models
#' and variables. The resulting object includes the following columns:
#' `model`, `variable`, `moderator_value`, `slope`, `SE`, `df`,
#' `lower_ci`, `upper_ci`, and `p.value`.
#' Returns `NULL` if no valid results are obtained.
#'
#' @examples
#' \dontrun{
#' models <- list(
#'   M1 = list(post_mlm = list(model = m1)),
#'   M2 = list(post_mlm = list(model = m2))
#' )
#'
#' tidy_emtrends(
#'   model_list = models,
#'   specs = "Moderator",
#'   vars = c("Predictor1", "Predictor2"),
#'   model_path = "post_mlm$model"
#' )
#' }
#'
#' @seealso [tidy_emtrend()], [emmeans::emtrends()]
#' @export
tidy_emtrends <- function(model_list,
                          specs,
                          vars, #a character vector of variables I want to
                          #probe interactions for
                          model_path
) {
  results <- vector("list")

  # Loop over models
  for (i in seq_along(model_list)) {

    #grab model short-hand name
    model_name <- names(model_list[i])

    #grab model
    model <- model_list[[i]][[model_path]]

    inner_results <- vector("list")
    # Loop over variables
    for (i1 in seq_along(vars)) {

      v <- vars[i1] #grab the specific variable name

      tmp <- tidy_emtrend(model,
                          specs = specs,
                          var = v,
                          mod_name = model_name)
      if (!is.null(tmp)) {
        #store the results nested under the specific variable and iteration
        inner_results[[i1]] <- tmp
      }


    }
    # bind together results for this model
    if (length(inner_results) > 0) {
      results[[i]] <- do.call(rbind, inner_results)
    }
  }

  # Combine all non-NULL results
  if (length(results) == 0) return(NULL)
  #bind together all of the information from different models
  results <- do.call(rbind, results)
  return(results)
}










