#' @importFrom psych unidim omega
NULL



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

    reliabilility_by_i <- scale_reliability(data, scale_i, group, groups, omega_h = omega_h)

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


#' Infer which latent factor is the general factor
#'
#' Picks the factor loading on the most items. Warns when the runner-up is
#' within one item, because that near-tie is where a silent guess would
#' mislabel a broad specific factor as the general one and quietly return
#' numbers for the wrong model.
#'
#' @param loadings Data frame of `=~` rows with `lhs`/`rhs`/`est.std` columns.
#' @param factors Character vector of unique latent factor names.
#' @return A single factor name.
#' @noRd
.detect_general_factor <- function(loadings, factors) {

  if (!length(factors)) {
    stop("No latent factors found in `fit`; omega_specific() needs a ",
         "bifactor model with a general factor and at least one specific ",
         "factor.", call. = FALSE)
  }

  if (length(factors) < 2L) {
    stop("Only one latent factor ('", factors[1], "') found in `fit`; a ",
         "bifactor model needs a general factor plus at least one specific ",
         "factor.", call. = FALSE)
  }

  n_items <- vapply(factors, function(f) sum(loadings$lhs == f), integer(1))
  ord <- order(n_items, decreasing = TRUE)
  best <- factors[ord[1]]

  if (n_items[ord[1]] - n_items[ord[2]] <= 1L) {
    warning("Auto-detected '", best, "' as the general factor (loads on ",
            n_items[ord[1]], " items), but '", factors[ord[2]], "' loads on ",
            n_items[ord[2]], ". That margin is too narrow to infer from ",
            "safely. Pass `general` explicitly to remove the guess.",
            call. = FALSE)
  }

  best
}


#' Omega-specific reliability for a bifactor measurement model
#'
#' @description
#' Computes \eqn{\omega_s} ("omega specific" / "omega subscale"), the
#' proportion of variance in a specific-factor item composite that is
#' attributable to that specific factor, from a fitted bifactor confirmatory
#' factor model. Unlike \code{\link{scale_reliability}}, which computes
#' reliability for a single unidimensional (or hierarchical) scale via
#' \code{psych::omega()}, this function reads standardized loadings straight
#' out of a fitted \code{lavaan} bifactor CFA.
#'
#' @param fit A fitted `lavaan` object (from \code{lavaan::cfa()} or
#'   \code{lavaan::sem()}) specifying a bifactor model: one general factor
#'   loading on the items, plus one or more orthogonal specific (group)
#'   factors, each loading on a subset of items.
#' @param general Character string giving the name of the general factor as
#'   it appears on the left-hand side of its `=~` lines in the model syntax.
#'   If `NULL` (default), the factor loading on the most items is used, with
#'   a warning when that choice is close enough to be a guess (see Details).
#' @param specific Optional character vector of specific-factor names to
#'   compute `omega_specific` for. If `NULL` (default), every latent factor
#'   in `fit` other than `general` is treated as a specific factor.
#'
#' @details
#' When `general` is not supplied it is inferred as the factor loading on the
#' most items, which is correct for any well-specified bifactor model. That
#' inference is only safe when it is unambiguous, so if the runner-up factor
#' loads on the same number of items or only one fewer, the function warns
#' and names both candidates. That near-tie is exactly the situation — a
#' broad specific factor, or a mis-specified model with two general-ish
#' factors — where a silent guess would mislabel a factor and return numbers
#' that look plausible but answer the wrong question. Pass `general`
#' explicitly to remove the guess entirely.
#'
#' The specific factors are always auto-detected from whatever other latent
#' factors exist in the model; getting that wrong only omits or adds a row to
#' the result rather than silently swapping roles.
#'
#' Loadings are taken from `lavaan::standardizedSolution(fit)`. For each
#' specific factor \eqn{s} with items \eqn{i \in s}:
#' \deqn{\omega_s = \frac{\left(\sum_{i \in s} \lambda_{s,i}\right)^2}{
#'   \left(\sum_{i \in s} \lambda_{g,i}\right)^2 +
#'   \left(\sum_{i \in s} \lambda_{s,i}\right)^2 +
#'   \sum_{i \in s} \theta_i}}
#' where \eqn{\lambda_{g,i}} and \eqn{\lambda_{s,i}} are item \eqn{i}'s
#' standardized loadings on the general factor and on specific factor
#' \eqn{s}, and \eqn{\theta_i = 1 - \lambda_{g,i}^2 - \lambda_{s,i}^2} is its
#' unique/residual variance (Rodriguez, Reise, & Haviland, 2016, p. 141,
#' Equation 4). Every item that loads on a given specific factor must also
#' load on the general factor, or the function errors — a bifactor item
#' without a general loading isn't something this formula was written to
#' handle.
#'
#' @return A data frame with one row per specific factor and columns
#'   `specific_factor`, `n_items`, and `omega_specific`.
#'
#' @references
#' Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Evaluating bifactor
#' models: Calculating and interpreting statistical indices. *Psychological
#' Methods, 21*(2), 137-150. \doi{10.1037/met0000045}
#'
#' @examples
#' \dontrun{
#' model <- '
#'   g  =~ i1 + i2 + i3 + i4 + i5 + i6
#'   s1 =~ i1 + i2 + i3
#'   s2 =~ i4 + i5 + i6
#' '
#' fit <- lavaan::cfa(model, data = my_data, orthogonal = TRUE, std.lv = TRUE)
#' omega_specific(fit, general = "g")
#' }
#'
#' @seealso [scale_reliability()] for alpha/omega on a single unidimensional
#'   scale.
#' @importFrom lavaan standardizedSolution
#' @export
omega_specific <- function(fit, general = NULL, specific = NULL) {

  if (!inherits(fit, "lavaan")) {
    stop("`fit` must be a fitted lavaan model (e.g. from lavaan::cfa()).",
         call. = FALSE)
  }

  if (!is.null(general) &&
      (!is.character(general) || length(general) != 1L || is.na(general))) {
    stop("`general` must be a single character string naming the general ",
         "factor, e.g. general = \"g\".", call. = FALSE)
  }

  std <- lavaan::standardizedSolution(fit)
  loadings <- std[std$op == "=~", c("lhs", "rhs", "est.std")]
  factors <- unique(loadings$lhs)

  if (is.null(general)) general <- .detect_general_factor(loadings, factors)

  if (!(general %in% factors)) {
    stop("`general` ('", general, "') is not a latent factor in `fit`. ",
         "Factors found: ", paste(factors, collapse = ", "), ".",
         call. = FALSE)
  }

  if (is.null(specific)) {
    specific <- setdiff(factors, general)
  } else {
    if (!is.character(specific)) {
      stop("`specific` must be a character vector of factor names, or NULL.",
           call. = FALSE)
    }
    bad <- setdiff(specific, factors)
    if (length(bad) > 0L) {
      stop("`specific` includes factor(s) not found in `fit`: ",
           paste(bad, collapse = ", "), ".", call. = FALSE)
    }
    if (general %in% specific) {
      stop("`specific` includes '", general, "', which is also `general`. ",
           "A factor cannot be its own specific factor.", call. = FALSE)
    }
  }

  if (length(specific) == 0L) {
    stop("No specific factors found besides `general` ('", general, "'); ",
         "a bifactor model needs at least one specific factor.",
         call. = FALSE)
  }

  general_loadings <- stats::setNames(
    loadings$est.std[loadings$lhs == general],
    loadings$rhs[loadings$lhs == general]
  )

  rows <- lapply(specific, function(s) {

    s_rows <- loadings[loadings$lhs == s, ]
    items <- s_rows$rhs
    lambda_s <- stats::setNames(s_rows$est.std, items)

    missing_general <- setdiff(items, names(general_loadings))
    if (length(missing_general) > 0L) {
      stop("Item(s) ", paste(missing_general, collapse = ", "),
           " load on specific factor '", s, "' but not on the general ",
           "factor '", general, "'. Every item on a specific factor must ",
           "also load on the general factor for omega_specific to be ",
           "defined.", call. = FALSE)
    }

    lambda_g <- general_loadings[items]
    theta <- 1 - lambda_g^2 - lambda_s^2

    sum_lg <- sum(lambda_g)
    sum_ls <- sum(lambda_s)
    sum_theta <- sum(theta)

    omega_s <- sum_ls^2 / (sum_lg^2 + sum_ls^2 + sum_theta)

    data.frame(specific_factor = s, n_items = length(items),
               omega_specific = omega_s, stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

