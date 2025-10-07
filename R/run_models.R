
#' Fit a List of Models (lm or lmer) and Attach Results
#'
#' Iterates over a \code{model_list}, fitting either \code{\link[stats]{lm}} or
#' \code{\link[lme4]{lmer}} depending on whether random effects are specified.
#' Each successfully fitted entry is augmented with:
#' \itemize{
#'   \item \code{$mr}: the fitted model object (\code{lm} or \code{lmerMod})
#'   \item \code{$ms}: the model summary (\code{summary.lm} or \code{summary.merMod})
#' }
#' Entries without a valid \code{$formula} are skipped with a message.
#'
#' @param model_list A list where each element describes one model. Each element
#'   should contain:
#'   \itemize{
#'     \item \code{$name} (optional) Character; used only for messaging.
#'     \item \code{$formula} A \code{formula} object to be fitted.
#'     \item \code{$data} Character scalar; the name of a data.frame in the environment (retrieved via \code{get()}).
#'     \item \code{$random_effects} Optional character vector; if \emph{absent or NULL}, \code{lm()} is used; otherwise \code{lmer()} is used.
#'   }
#'
#' @details
#' The function treats the presence of \code{$random_effects} as a switch:
#' if it is \code{NULL}, the function calls \code{stats::lm()}; otherwise it calls \code{lme4::lmer()}.
#' The data frame is resolved as \code{get(model_list[[i]]$data)} in the calling environment.
#'
#' @return The input \code{model_list}, with fitted objects (\code{$mr}) and summaries (\code{$ms}) attached
#'   for each model that was successfully fitted. Skipped entries are returned unchanged.
#'
#' @examples
#' \dontrun{
#' # Assume df exists with y, x1, x2, id
#' ml <- list(
#'   fixed_only = list(
#'     name = "Fixed-only model",
#'     formula = y ~ x1 + x2,
#'     data = "df"  # name of data in environment
#'   ),
#'   mixed = list(
#'     name = "Mixed model",
#'     formula = y ~ x1 + (1 + x1 | id),
#'     data = "df",
#'     random_effects = c("1", "x1")
#'   )
#' )
#' out <- run_models(ml)
#' out$fixed_only$ms
#' out$mixed$ms
#' }
#'
#' @importFrom stats lm
#' @importFrom lme4 lmer
#' @export

run_models <- function(model_list) {

  for (i in seq_along(model_list)) {

    if(is.null(model_list[[i]]$formula)) {
      message(model_list[i], model_list[[i]]$name, ": did not have a formula specified
            and was skipped")
      next} #skip model if the formula is null

    #grabbing dataframe from environment for model
    df <- get(model_list[[i]]$data)

    #using null random effects as a filter to fit a
    #lm() model instead of the lmer model. If random_effects are provided,
    #the lmer() is fit.
    if(is.null(model_list[[i]]$random_effects)) {

      fit <- lm(model_list[[i]]$formula, data = df)
      summary <- summary(fit)

    } else {#in other words, if there is a random_effects term or grouping_variable
      #defined

      fit <- lmer(model_list[[i]]$formula, data = df)
      summary <- summary(fit)

    }

    #add fit and summary of fit to list
    model_list[[i]]$mr <- fit
    model_list[[i]]$ms <- summary

  }
  return(model_list)
}
