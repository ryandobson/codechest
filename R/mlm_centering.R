#utils::globalVariables(c(""))


#' Group-Mean Centering for Multiple Variables (Base R)
#'
#' Performs group-mean centering for one or more variables in a data frame.
#' For each variable, this function computes the between-group mean and
#' within-group deviation relative to a specified grouping variable, and
#' optionally creates standardized (z-scored) versions of these quantities.
#'
#' Each derived variable automatically receives an informative label,
#' even if the original variable had none. If a `"label"` attribute exists,
#' it is included in the new label; otherwise, the variable name is used.
#'
#' @param df A data frame in long format containing the variables to process.
#' @param variables A character vector of variable names to be group-mean centered.
#' @param group A string giving the name of the grouping variable that defines clusters or groups.
#' @param within_affix A string used to label within-group variables (default = `"WG"`).
#' @param between_affix A string used to label between-group variables (default = `"BG"`).
#' @param affix_type A string indicating whether to add affixes as `"prefix"` or `"suffix"`.
#'   Defaults to `"prefix"`. If another value is supplied, the function will raise an error.
#' @param include_z Logical; whether to include standardized (z-scored) versions
#'   of each between- and within-group variable. Defaults to `TRUE`.
#'
#' @importFrom stats ave
#'
#' @details
#' For each variable in `variables`, the function:
#' \enumerate{
#'   \item Computes the between-group mean (average of the variable within each group).
#'   \item Computes the within-group deviation (individual score minus its group mean).
#'   \item Optionally standardizes both using \code{scale()}.
#'   \item Creates or updates `"label"` attributes for all derived variables:
#'     \itemize{
#'       \item \emph{Between-group mean:} `"Between-group mean of <group>: <label or variable name>"`
#'       \item \emph{Within-group deviation:} `"Within-group deviation of <group>: <label or variable name>"`
#'       \item \emph{Z-scored versions:} `"Z-scored between-group mean of <group>: <label or variable name>"`, etc.
#'     }
#' }
#'
#' @return
#' The input data frame with new variables added for each input variable:
#' \itemize{
#'   \item Between-group mean: `"BG_<variable>"` or `"<variable>_BG"`
#'   \item Within-group deviation: `"WG_<variable>"` or `"<variable>_WG"`
#'   \item (optional) Standardized versions: `"ZBG_<variable>"`, `"ZWG_<variable>"`, etc.
#' }
#' Each new variable receives a descriptive `"label"` attribute for interpretability.
#'
#' @examples
#' df <- data.frame(
#'   id = rep(1:3, each = 3),
#'   score = c(4, 5, 6, 3, 2, 1, 5, 7, 6)
#' )
#'
#' attr(df$score, "label") <- "Self-rated performance"
#'
#' df2 <- mlm_groupmean(df,
#'                      variables = "score",
#'                      group = "id",
#'                      affix_type = "prefix",
#'                      include_z = TRUE)
#'
#' attr(df2$BG_score, "label")
#' #> "Between-group mean of id: Self-rated performance"
#'
#' attr(df2$WG_score, "label")
#' #> "Within-group deviation of id: Self-rated performance"
#'
#' @seealso
#' \code{\link{ave}}, \code{\link{scale}}
#'
#' @export

mlm_groupmean <- function(df, variables, group,
                          within_affix = "WG",
                          between_affix = "BG",
                          affix_type = c("prefix", "suffix"),
                          include_z = TRUE) {
  #> WG = within-group
  #> BG = between-group

  #> match.arg chooses the first argument of the vector automatically (prefix, here)
  #> and flashes and error if an incorrect name is provided.
  affix_type <- match.arg(affix_type)

  for (i in seq_along(variables)) {

    #grab the variable name to put into arugments
    variable_i <- variables[[i]]

    # Construct variable names
    if (affix_type == "prefix") {
      bg_name  <- paste0(between_affix, "_", variable_i)
      wg_name  <- paste0(within_affix, "_", variable_i)
      zbg_name <- paste0("Z", between_affix, "_", variable_i)
      zwg_name <- paste0("Z", within_affix, "_", variable_i)
    } else {
      bg_name  <- paste0(variable_i, "_", between_affix)
      wg_name  <- paste0(variable_i, "_", within_affix)
      zbg_name <- paste0(variable_i, "_Z", between_affix)
      zwg_name <- paste0(variable_i, "_Z", within_affix)
    }

    df[[bg_name]] <- ave(df[[variable_i]], df[[group]], FUN = function(xi) mean(xi, na.rm = TRUE))
    df[[wg_name]] <- df[[variable_i]] - df[[bg_name]]


    #> standardized variables if desired
    if (isTRUE(include_z)) {

      df[[zbg_name]] <- scale(df[[bg_name]])[,1]
      df[[zwg_name]] <- scale(df[[wg_name]])[,1]

    }

    # Transfer or create labels
    orig_label <- attr(df[[variable_i]], "label")
    if (is.null(orig_label)) orig_label <- variable_i #if no original lable, save the
    #label as the variable name

    attr(df[[bg_name]],  "label") <- paste0("Between-group mean of ", group, ": ", orig_label)
    attr(df[[wg_name]],  "label") <- paste0("Within-group deviation of ", group, ": ", orig_label)
    if (isTRUE(include_z)) {
      attr(df[[zbg_name]], "label") <- paste0("Z-scored between-group mean of ", group, ": ", orig_label)
      attr(df[[zwg_name]], "label") <- paste0("Z-scored within-group deviation of ", group, ": ", orig_label)
    }



  } #end of for loop

  return(df)

}
