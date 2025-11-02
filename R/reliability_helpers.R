utils::globalVariables(c("unidim", "omega"))


#' Compute reliability for a single psychological scale
#'
#' @description
#' Computes internal consistency and unidimensionality diagnostics for a single
#' psychological scale, optionally separated by a grouping variable.
#' Returns both `unidim()` and `psych::omega()` outputs.
#'
#' @param data A data frame containing the items for the scale.
#' @param vars Character vector of item variable names to include in the scale.
#' @param group Optional; a single character string naming the grouping variable.
#'   If supplied, must also provide \code{groups}.
#' @param groups Optional; character vector of the group levels to include
#'   (e.g., \code{c("male","female")}). Must be provided when \code{group} is used.
#' @param omega_h Logical; whether to compute hierarchical omega (\code{TRUE})
#'   or total omega (\code{FALSE}, default).
#'
#' @details
#' When \code{group} and \code{groups} are both \code{NULL}, reliability is
#' computed for the full sample. When both are provided, reliability is computed
#' separately within each group level.
#'
#' @return A list containing:
#'   \itemize{
#'     \item For ungrouped data: \code{list(uni, omega)}
#'     \item For grouped data: A named list of such results, one per group
#'   }
#'
#' @examples
#' \dontrun{
#' # Single group
#' scale_reliability(df, vars = c("item1","item2","item3"))
#'
#' # By sex
#' scale_reliability(df, vars = c("item1","item2","item3"),
#'                   group = "sex", groups = c("male","female"))
#' }
#'
#' @export
scale_reliability <- function(data,
                              vars,
                              group = NULL,
                              groups = NULL,
                              omega_h = FALSE) {


  if(is.null(group) && !is.null(groups)) {
    stop("You provided a group, but did not specify the group levels.
         e.g., group = sex, groups = c('male', 'female'). Please provide both
         a group and its groups, or niether for reliability across groups")
  }


  if(is.null(group) && is.null(groups)) {

    uni <- unidim(data[, vars])

    if(omega_h == FALSE) {
      omeg <- omega(data[, vars], nfactors = 1, fm = "ml")
    }

    if(omega_h == TRUE) {
      omeg <- omega(data[, vars], nfactors = 3, fm = "ml")
    }

    reliability <- list(
      uni = uni,
      omega = omeg
    )
    return(reliability)

  }



  if(!is.null(group) && !is.null(groups)) {
    grouped_reliability <- vector("list", length(groups))
    names(grouped_reliability) <- groups
    for (i in groups) {

      group_i <- data[data[[group]] == i, vars]

      uni_i <- unidim(group_i)

      if(omega_h == FALSE) {
        omega_i <- omega(group_i, nfactors = 1, fm = "ml")
      }

      if(omega_h == TRUE) {
        omega_i <- omega(group_i, nfactors = 3, fm = "ml")
      }


      grouped_reliability[[i]] <- # reliability_i
        list(uni = uni_i,
             omega = omega_i)
    } #end of for loop

    return(grouped_reliability)

  } #end of if statement for grouped reliability


}


#' Compute reliabilities for multiple scales
#'
#' @description
#' Applies \code{\link{scale_reliability}} across a list of scales to compute
#' reliability indices for each one, optionally within groups.
#'
#' @param data A data frame containing all item variables.
#' @param scales A named list where each element is a character vector of item names.
#' @param group Optional; grouping variable name passed to \code{scale_reliability()}.
#' @param groups Optional; specific factor levels of the grouping variable.
#' @param omega_h Logical; whether to compute hierarchical omega (default = \code{FALSE}).
#'
#' @return A named list of reliability objects (see \code{\link{scale_reliability}} for details).
#'
#' @examples
#' \dontrun{
#' scales <- list(
#'   empathy = c("emp1","emp2","emp3"),
#'   trust   = c("trust1","trust2","trust3")
#' )
#' scale_reliabilities(df, scales, group = "sex", groups = c("male","female"))
#' }
#'
#' @export
scale_reliabilities <- function(data, scales, group = NULL, groups = NULL, omega_h = FALSE) {

  #> data = df with all of the necessary variables
  #> scales = a list of scales
  #> group = the grouping variable
  #> groups = the specific factor levels of the group (can be more than 2)

  scale_reliability <- vector("list", length(scales))
  names(scale_reliability) <- names(scales)
  for(i in seq_along(scales)) {

    #grab the specific scale variables from the list
    scale_i <- scales[[i]]

    reliabilility_by_i <- scale_reliability(data, scale_i, group, groups, omega_h = FALSE)

    #grab the specific name to ensure its assigned correctly
    scale_name <- names(scales[i])

    scale_reliability[[scale_name]] <- reliabilility_by_i
  }

  return(scale_reliability)
}


#' Tidy output from \code{scale_reliability()}
#'
#' @description
#' Extracts and tidies reliability indices (unidimensionality, Cronbach's alpha,
#' and total omega) from the output of \code{\link{scale_reliability}}.
#' Handles both grouped and ungrouped results.
#'
#' @param scale_reliability_output Output object from \code{scale_reliability()}.
#' @param rnd Number of decimal places to round numeric results (default = 2).
#'
#' @return A data frame with columns:
#'   \code{group}, \code{u}, \code{alpha}, and \code{omega_tot}.
#'   When ungrouped, \code{group} is "overall".
#'
#' @examples
#' \dontrun{
#' rel <- scale_reliability(df, c("item1","item2","item3"))
#' tidy_scale_reliability(rel)
#' }
#'
#' @export

tidy_scale_reliability <- function(scale_reliability_output, rnd = 2) {


  #duplicate input name for ease of reference throughout function
  rel <- scale_reliability_output


  # --- Handle ungrouped reliability (single list with $uni and $omega) ---
  if (all(c("uni", "omega") %in% names(rel))) {
    uni_i <- rel$uni$uni["u"]
    alpha_i <- rel$uni$uni["alpha"]
    omega_tot_i <- rel$omega$omega.tot

    uni_results <- data.frame(
      group = "overall",
      u = uni_i,
      alpha = alpha_i,
      omega_tot = omega_tot_i
    )

    # rounding
    if (rnd != FALSE) {
      uni_results[] <- lapply(
        uni_results,
        function(x) if (is.double(x)) round(x, digits = rnd) else x
      )
    }

    return(uni_results)
  }

  # --- Handle grouped reliability (list of groups, each with $uni and $omega) ---
  #grab the names of the groups that are stored in the object
  groups <- names(scale_reliability_output)

  #initialize a list to store results

  uni_results <- vector("list", length(rel))
  names(uni_results) <- names(rel)
  for (group in groups) {

    #grab in the actual dataframe object for uni info (includes alpha and more)
    uni_i <- rel[[group]]$uni$uni["u"]
    alpha_i <- rel[[group]]$uni$uni["alpha"]

    omega_tot_i <- rel[[group]]$omega$omega.tot

    #add the specific group to the data for later reference
    #uni_i$group <- group

    uni_results[[group]] <- data.frame(
      group = group,
      u = uni_i,
      alpha = alpha_i,
      omega_tot = omega_tot_i
    )

  }

  uni_results <- do.call(rbind, uni_results)
  rownames(uni_results) <- NULL
  #> round the results
  if (rnd != FALSE) {
    uni_results[] <- lapply(uni_results, function(x) if(is.double(x)) round(x, digits = rnd) else x)
  }

  return(uni_results)
}



#' Tidy output from \code{scale_reliabilities()}
#'
#' @description
#' Extracts and combines reliability summaries across multiple scales and groups,
#' returning a long-format data frame.
#'
#' @param scale_reliabilities_output Output from \code{scale_reliabilities()}.
#' @param rnd Number of decimal places to round numeric results (default = 2).
#'
#' @return A data frame with columns:
#'   \code{scale}, \code{group}, \code{u}, \code{alpha}, and \code{omega_tot}.
#'
#' @examples
#' \dontrun{
#' rels <- scale_reliabilities(df, scales = my_scales)
#' tidy_scale_reliabilities(rels)
#' }
#'
#' @export

tidy_scale_reliabilities <- function(scale_reliabilities_output, rnd = 2) {


  #> Now I'm working with a list of reliaiblites by scale, which are calculated
  #> by groups

  rel <- scale_reliabilities_output

  #> grab the scale names which are at the top of the list output
  scales <- names(rel)

  scale_uni_results <- vector("list", length(rel))
  names(scale_uni_results) <- names(rel)

  for(scale in scales) {

    scale_uni_i <- tidy_scale_reliability(rel[[scale]], rnd)

    #add in the scale to the dataframe
    scale_uni_i$scale <- scale

    #save the new values to the dataframe
    scale_uni_results[[scale]] <- scale_uni_i

  }

  uni_results <- do.call(rbind, scale_uni_results)
  rownames(uni_results) <- NULL

  uni_results <- uni_results[ , c("scale", "group", "u", "alpha", "omega_tot")]

  return(uni_results)
}

