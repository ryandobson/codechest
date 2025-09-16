utils::globalVariables(c("everything", "variables", "stats", "cor", "setNames"))



#' Generate a Nice-Looking Correlation Matrix with Custom Bolded Values
#'
#' This function takes a correlation matrix and formats it into a nicely styled HTML table,
#' with the ability to bold correlation coefficients above and below a specified threshold (`bold_val`).
#' It supports bolding values greater than the threshold, less than the negative threshold, or equal to 1 (diagonal).
#'
#' @param cor_mat A numeric correlation matrix (data frame or matrix) with numeric values representing the correlation coefficients.
#' @param bold_val A numeric value that sets the threshold for bolding correlations. Correlations greater than this value (or less than the negative value) will be bolded.
#' default = .30
#'
#' @return A styled HTML table representing the correlation matrix with bolded values based on the specified threshold.
#'
#' @details
#' The function generates a correlation matrix where the correlation coefficients that exceed the specified `bold_val`
#' (in absolute value) are bolded. This can be useful for highlighting strong correlations in a matrix. The matrix is
#' displayed using `kableExtra` to produce a polished HTML table. Diagonal elements (which are always 1) are bolded by default,
#' but this can be adjusted by modifying the `ifelse` condition inside the `mutate()` function.
#'
#' @importFrom dplyr mutate across where
#' @importFrom tibble as_tibble
#' @importFrom kableExtra kbl kable_paper cell_spec
#' @export
#'
#' @examples
#' # Example usage of cor_mat_bold_fun
#' \donttest{
#' cor_matrix <- cor(mtcars)
#' cor_format(cor_matrix, bold_val = 0.3)
#' }
cor_format <- function(cor_mat, bold_val = .30) {

  cor_mat |>
    tibble::as_tibble() |>
    dplyr::mutate(variables = colnames(cor_mat)) |> # Adding the variable names on the y-axis
    dplyr::select(variables, everything()) |> # Moving variable name to the front of the data frame
    dplyr::mutate(
      across(where(is.double), ~ round(., 2)),
      across(where(is.double), ~ kableExtra::cell_spec(., bold = ifelse(. >= bold_val | . <= - bold_val | . == 1, TRUE, FALSE))) # Bold values based on threshold
    ) |>
    kableExtra::kbl(format = "html", escape = FALSE) |>
    kableExtra::kable_paper("striped")

}

#' Calculate and Summarize Correlation Statistics
#'
#' This function computes several summary statistics about the correlations in a given correlation matrix:
#' - Total number of correlations (off-diagonal elements only).
#' - Number of correlations with absolute values greater than 0.30.
#' - The percentage of correlations that exceed the absolute value of 0.30 in the correlation matrix.
#'
#' @param df A data frame containing the variables to compute correlations between. The function uses the number of columns in the data frame to calculate the total number of correlations.
#' @param cor_mat A correlation matrix (numeric matrix) containing the correlation coefficients between variables.
#'
#' @return A named vector containing:
#' \describe{
#'   \item{`Total number of correlations`}{The total number of off-diagonal correlations in the matrix.}
#'   \item{`Number of correlations larger than abs(.30)`}{The number of off-diagonal correlations with an absolute value greater than or equal to 0.30.}
#'   \item{`Percent of correlations larger than abs(.30) in cor matrix`}{The percentage of off-diagonal correlations greater than or equal to 0.30, as a proportion of the total number of correlations.}
#' }
#'
#' @details
#' This function operates on a correlation matrix (`cor_mat`) and calculates:
#' - The **total number of correlations** (off-diagonal values only).
#' - The **number of correlations** where the absolute value exceeds 0.30, which is a typical threshold to identify moderate or stronger relationships.
#' - The **percentage of correlations** that are above the threshold.
#'
#' The function is useful for understanding the strength of correlations in a correlation matrix and for filtering out weaker relationships.
#'
#' @export
#'
#' @examples
#' # Example usage of cor_nums
#' cor_matrix <- cor(mtcars)
#' cor_nums(mtcars, cor_matrix)
cor_nums <- function(df, cor_mat) {

  # Total number of correlations (off-diagonal elements only)
  totr <- (length(df) * (length(df) - 1)) / 2
  #> NOTE: If I wanted unique pieces of information (i.e., include the variances),
  #> then I would change the "- 1" to "+ 1".

  # Logical matrix to identify off-diagonal elements
  off_diag <- row(cor_mat) != col(cor_mat)

  # Number of correlations above abs(.30)
  r30 <- sum(abs(cor_mat[off_diag]) >= .30) / 2

  # Percent of off-diagonal elements > abs(.30) in cor matrix
  rperc <- round((r30 / totr) * 100, 1)

  # Create the final result as a named vector
  final <- c(
    "Total number of correlations" = totr,
    "Number of correlations larger than abs(.30)" = r30,
    "Percent of correlations larger than abs(.30) in cor matrix" = rperc
  )

  return(final)
}



#' Compute Group-Specific Correlation Matrices
#'
#' This function computes correlation matrices separately for two groups
#' within a dataset. It allows specifying the variables for correlation
#' and the grouping variable, and returns a named list of two correlation
#' matrices corresponding to the groups.
#'
#' @param df A data frame containing the variables of interest.
#' @param cor_vars A character vector of variable names to include in the
#'   correlation analysis.
#' @param group_var The grouping variable, supplied as either a bare name
#'   or a string. This variable must exist in \code{df}.
#' @param groups A character vector of length two specifying the two levels
#'   of \code{group_var} to split the data by (e.g.,
#'   \code{c("male", "female")}).
#' @param cor_use Character string indicating the handling of missing values,
#'   passed to the \code{use} argument of [stats::cor()]. Defaults to
#'   \code{"pairwise"}.
#'
#' @return A named list of two correlation matrices, where each element
#'   corresponds to one of the groups supplied in \code{groups}.
#'
#' @details
#' The function checks that \code{groups} has exactly two elements,
#' that \code{group_var} exists in the data, and that both group levels
#' are present. If these conditions are not met, an error is thrown.
#'
#' @examples
#' # Example with the iris dataset
#' compute_split_cors(
#'   df = iris,
#'   cor_vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'   group_var = "Species",
#'   groups = c("setosa", "versicolor")
#' )
#'
#' @export

compute_split_cors <- function(df, cor_vars, group_var, groups, cor_use = "pairwise") {

  #> df = dataframe with all variables
  #> cor_vars = the selection of variables for correlational analysis
  #> group_var = the name of the grouping variable to split analysis by (e.g., sex)
  #> can be input as a string or bare name
  #> groups = a vector of two lengths, where it is the levels of the grouping variable
  #> e.g., if sex is the group_var, the groups might be c("male", "female")
  #> cor_use = "pairwise" - This is the "use =" method from the cor() function


  # capture bare or string: group_var or "group_var"
  #gv <- rlang::as_name(rlang::ensym(group_var))

  #> base r version of allowing bare name or string as input for group_var
  expr <- substitute(group_var)
  gv <- if (is.symbol(expr)) deparse(expr) else as.character(group_var)

  # --- input checks ---
  if (length(groups) != 2L) stop("`groups` must be a character vector of length 2.")
  if (!gv %in% names(df)) stop(sprintf("`group_var` %s not found in df.", shQuote(gv)))
  if (!all(groups %in% unique(df[[gv]]))) {
    missing_levels <- setdiff(groups, unique(df[[gv]]))
    stop("These `groups` are not present in `group_var`: ",
         paste(missing_levels, collapse = ", "))
  }

  #> get two separate dataframes that include only the correlation variables
  #> one dataframe is one level of the group_var, the other dataframe is the second
  #> level of the group_var
  df1 <- df[df[[gv]] == groups[1], cor_vars]
  df2 <- df[df[[gv]] == groups[2], cor_vars]

  #> compute correlation matrices by group
  cor1 <- cor(df1, use = cor_use)
  cor2 <- cor(df2, use = cor_use)

  #name matrices based on the grouping variable and output
  out <- setNames(list(cor1, cor2), groups)

  return(out)
}






#' Rename Rows and Columns of a Correlation Matrix
#'
#' This function renames the row and column names of a correlation matrix
#' using a user-supplied vector of old-to-new name mappings. The renaming
#' is "safe": if some names are not found in the mapping, the original
#' names are preserved. The function also optionally rounds the
#' correlation values.
#'
#' @param cor_mat A correlation matrix, typically produced by [stats::cor()].
#' @param rename_vec A named character vector of name mappings, where the
#'   names correspond to the old variable names and the values correspond
#'   to the new names. For example:
#'   \code{c(old_name1 = "New Name 1", old_name2 = "New Name 2")}.
#'   Extra mappings not present in \code{cor_mat} are ignored without error.
#' @param rnd Integer specifying the number of decimal places to round to.
#'   Defaults to 2. Set to \code{FALSE} or \code{NULL} to skip rounding.
#'
#' @return A correlation matrix with updated row and column names.
#'   If \code{rnd} is specified, the matrix values are rounded.
#'
#' @details
#' The function uses an internal helper (`safe_rename`) to apply
#' the renaming. If all current names already match the new names,
#' no changes are made. If some names are missing from the mapping,
#' they are left unchanged.
#'
#' @examples
#' mat <- cor(mtcars[, 1:3])
#' rename_vec <- c(mpg = "Miles Per Gallon", cyl = "Cylinders")
#' rename_cor(mat, rename_vec, rnd = 3)
#'
#' @export

rename_cor <- function(cor_mat, rename_vec, rnd = 2) {
  #cor_matrix = correlation matrix, usually produced by "cor()"
  #rename_vec = a renaming vector that specifies the updated names
  #> Created by doing rename_vec <- c(old_name1 = "New Name 1",
  #old_name2 = "New Name 2")
  #
  #> NOTE: The rename_vec can contain additional variables that I don't have in
  #> the correlation matrix and this function still works.

  # Helper: safely rename one side (cols or rows)
  safe_rename <- function(nms, map) {
    # If all names are already in the map's values (new names), return as-is
    if (all(nms %in% unname(map))) {
      return(nms)
    }
    # Otherwise do the mapping
    out <- map[nms]
    # If map doesn't cover some names, keep originals
    out[is.na(out)] <- nms[is.na(out)]
    out
  }
  #> the safe_rename function takes two arguments:
  #> nms = the colnames of the correlation matrix as "colnames(cor_mat)"
  #> map = the rename_vec (same as supplied to the original function)

  #> run the safe_rename function here
  colnames(cor_mat) <- safe_rename(colnames(cor_mat), rename_vec)
  rownames(cor_mat) <- safe_rename(rownames(cor_mat), rename_vec)

  if (!is.null(rnd) && rnd != FALSE) {
    cor_mat <- round(cor_mat, rnd)
  }

  return(cor_mat)
}



#' Combine Upper and Lower Correlation Triangles
#'
#' Combines two square matrices by taking the strict upper triangle from one
#' and the strict lower triangle from the other. The diagonal can be taken from
#' either input, averaged, or set to a constant (1 or 0). Variable orders are
#' aligned by name before combining.
#'
#' @param upper_mat A square numeric matrix providing the strict upper triangle
#'   (above-diagonal) values. Must have row/column names.
#' @param lower_mat A square numeric matrix providing the strict lower triangle
#'   (below-diagonal) values. Must have row/column names and the same set of
#'   variable names as \code{upper_mat} (order may differ).
#' @param diag_from One of \code{"upper"}, \code{"lower"}, \code{"average"},
#'   \code{"one"}, or \code{"zero"} specifying how to fill the diagonal:
#'   \itemize{
#'     \item \code{"upper"}: use \code{diag(upper_mat)}
#'     \item \code{"lower"}: use \code{diag(lower_mat)}
#'     \item \code{"average"}: row-wise mean of the two diagonals (ignoring NAs)
#'     \item \code{"one"}: set diagonal to 1
#'     \item \code{"zero"}: set diagonal to 0
#'   }
#'
#' @return A square numeric matrix whose strict upper triangle comes from
#'   \code{upper_mat}, strict lower triangle from \code{lower_mat}, and diagonal
#'   determined by \code{diag_from}. Row names are suffixed with positional
#'   indices \code{" (1)", " (2)", ...}, and column names are set to character
#'   indices \code{"1","2",...}.
#'
#' @details
#' The function checks that both inputs are square matrices with names and the
#' same set of variables. It reorders \code{lower_mat} to match the column order
#' of \code{upper_mat} before combining. Only strict triangles are copied; the
#' diagonal is handled separately according to \code{diag_from}.
#'
#' @examples
#' set.seed(1)
#' vars <- c("A","B","C")
#' U <- matrix(runif(9), 3, 3, dimnames = list(vars, vars))
#' L <- matrix(runif(9), 3, 3, dimnames = list(vars, vars))
#' U[lower.tri(U, diag = TRUE)] <- NA_real_
#' L[upper.tri(L, diag = TRUE)] <- NA_real_
#' diag(U) <- 1; diag(L) <- 1
#'
#' combine_corr_triangles(U, L, diag_from = "upper")
#' combine_corr_triangles(U, L, diag_from = "average")
#' combine_corr_triangles(U, L, diag_from = "one")
#'
#' @export
combine_corr_triangles <- function(upper_mat, lower_mat,
                                   diag_from = c("upper", "lower", "average", "one", "zero")) {
  diag_from <- match.arg(diag_from)

  # --- Basic checks ---
  if (!is.matrix(upper_mat) || !is.matrix(lower_mat))
    stop("Both inputs must be matrices.")
  if (nrow(upper_mat) != ncol(upper_mat) || nrow(lower_mat) != ncol(lower_mat))
    stop("Both matrices must be square.")
  if (is.null(colnames(upper_mat)) || is.null(colnames(lower_mat)))
    stop("Both matrices must have row/column names.")
  if (!setequal(colnames(upper_mat), colnames(lower_mat)))
    stop("Matrices must contain the same variable names (possibly in different orders).")

  # --- Align lower_mat to upper_mat's order ---
  vars <- colnames(upper_mat)
  lower_mat <- lower_mat[vars, vars, drop = FALSE]
  rownames(upper_mat) <- colnames(upper_mat) <- vars
  rownames(lower_mat) <- colnames(lower_mat) <- vars

  # --- Build result ---
  n <- nrow(upper_mat)
  out <- matrix(NA_real_, n, n, dimnames = list(vars, vars))

  # Fill triangles
  out[upper.tri(out, diag = FALSE)] <- upper_mat[upper.tri(upper_mat, diag = FALSE)]
  out[lower.tri(out, diag = FALSE)] <- lower_mat[lower.tri(lower_mat, diag = FALSE)]

  # Diagonal handling
  diag(out) <- switch(
    diag_from,
    upper   = diag(upper_mat),
    lower   = diag(lower_mat),
    average = rowMeans(cbind(diag(upper_mat), diag(lower_mat)), na.rm = TRUE),
    one     = 1,
    zero    = 0
  )

  # --- Final naming step ---
  # Append " (1)", " (2)", ... to rownames; set colnames to "1","2",...
  orig_rn <- rownames(out)
  rownames(out) <- paste0(orig_rn, " (", seq_len(n), ")")
  colnames(out) <- as.character(seq_len(n))

  return(out)
}



#' Publish-Ready Correlation Table Combining Two Groups
#'
#' Computes group-specific correlation matrices, renames variables, combines
#' them into a single matrix with one group's correlations in the strict upper
#' triangle and the other's in the strict lower triangle, and attaches a note
#' indicating which group is where. Optionally sets/derives the diagonal and
#' rounds values. The returned object has class \code{"pub_cors_mat"} for
#' convenient downstream printing/export.
#'
#' @param df A data frame containing the variables of interest and the grouping variable.
#' @param cor_vars A character vector of variable names to include in the correlation analysis.
#' @param group_var The grouping variable, supplied as a bare name or a string. Must exist in \code{df}.
#' @param groups A character vector of length two giving the two levels of \code{group_var}
#'   to split the data by (e.g., \code{c("male", "female")}).
#' @param rename_vec A named character vector mapping old variable names (names of the vector)
#'   to new display names (values). Extra mappings not present in \code{cor_vars} are ignored.
#' @param diag_from How to fill the diagonal of the combined matrix. One of
#'   \code{"upper"}, \code{"lower"}, \code{"average"}, \code{"one"}, or \code{"zero"}.
#'   Defaults to \code{"one"}.
#' @param cor_use Character string passed to the \code{use} argument of [stats::cor()]
#'   (e.g., \code{"pairwise"}, \code{"complete.obs"}). Defaults to \code{"pairwise"}.
#' @param rnd Integer number of decimal places for rounding. Defaults to 2. Set to
#'   \code{FALSE} or \code{NULL} to skip rounding.
#'
#' @return A square numeric matrix of class \code{c("pub_cors_mat", "matrix")}.
#'   The strict upper triangle contains correlations from \code{groups[1]}, the strict
#'   lower triangle from \code{groups[2]}, and the diagonal is set according to
#'   \code{diag_from}. An attribute \code{attr(x, "note")} is added in the form
#'   \dQuote{Upper triangle = <group1>, Lower triangle = <group2>}. Row names are
#'   suffixed with indices \code{" (1)", " (2)", ...}, and column names are character
#'   indices \code{"1","2",...}, mirroring [combine_corr_triangles()].
#'
#' @details
#' Internally, this function:
#' \enumerate{
#'   \item splits \code{df} by \code{group_var} and computes two correlation matrices via
#'     [compute_split_cors()],
#'   \item renames rows/columns using [rename_cor()] with \code{rename_vec} and \code{rnd},
#'   \item merges triangles with [combine_corr_triangles()] using \code{diag_from},
#'   \item tags the result with class \code{"pub_cors_mat"} and a descriptive \code{"note"} attribute.
#' }
#'
#' @examples
#' # Minimal reproducible example
#' df <- iris
#' cor_vars <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
#' groups <- c("setosa","versicolor")
#' rename_vec <- c(
#'   Sepal.Length = "Sepal Length",
#'   Sepal.Width  = "Sepal Width",
#'   Petal.Length = "Petal Length",
#'   Petal.Width  = "Petal Width"
#' )
#'
#' tbl <- pub_cors(
#'   df, cor_vars,
#'   group_var = "Species",
#'   groups = groups,
#'   rename_vec = rename_vec,
#'   diag_from = "one",
#'   cor_use = "pairwise",
#'   rnd = 2
#' )
#' class(tbl)
#' attr(tbl, "note")
#'
#' @seealso [compute_split_cors()], [rename_cor()], [combine_corr_triangles()], [stats::cor()]
#'
#' @export

pub_cors <- function(df, cor_vars, group_var, groups,
                     rename_vec,
                     diag_from = "one",
                     cor_use = "pairwise",
                     rnd = 2) {


  #> base r version of allowing bare name or string as input for group_var
  expr <- substitute(group_var)
  gv <- if (is.symbol(expr)) deparse(expr) else as.character(group_var)

  cor_mats <- compute_split_cors(df, cor_vars, as.name(gv), groups, cor_use = cor_use)

  cor1 <- rename_cor(cor_mats[[1]], rename_vec, rnd = rnd)
  cor2 <- rename_cor(cor_mats[[2]], rename_vec, rnd = rnd)

  out <- combine_corr_triangles(cor1, cor2, diag_from = diag_from)

  #> Adding a custom note to the matrix so I can see what group is upper/lower
  attr(out, "note") <- paste0("Upper triangle = ", groups[1],
                              ", Lower triangle = ", groups[2])
  class(out) <- c("pub_cors_mat", class(out))  # add custom class

  return(out)

}



