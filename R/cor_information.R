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






#' Safely rename and round a correlation matrix
#'
#' Renames rows/columns of a correlation matrix using a name mapping, with
#' optional automatic flipping of the mapping direction (old→new vs. new→old),
#' optional rounding, and optional publication-style column labels ("1","2",...).
#'
#' @param cor_mat A square numeric correlation matrix (e.g., from [stats::cor()]).
#' @param rename_vec Optional named character vector for renaming.
#'   Accepts either \code{old_name = "New Label"} (preferred) or the reverse.
#'   Extra names in \code{rename_vec} that are not present in \code{cor_mat}
#'   are ignored.
#' @param rename_cols Logical; if \code{TRUE}, row names become
#'   \code{"<original> (1..n)"} and column names become \code{"1..n"}.
#' @param rnd Integer number of decimals to round to. Set \code{FALSE} to skip.
#'
#' @details
#' If none of the matrix names match \code{names(rename_vec)} but some match
#' \code{unname(rename_vec)}, the mapping is flipped automatically so the
#' function remains robust to \code{old→new} vs. \code{new→old} inputs.
#'
#' @return A correlation matrix with updated dimnames; possibly rounded and with
#'   publication-style numbering if \code{rename_cols = TRUE}.
#'
#' @examples
#' set.seed(1)
#' m <- cor(matrix(rnorm(50), ncol = 5))
#' dimnames(m) <- list(letters[1:5], letters[1:5])
#' map <- c(a = "Alpha", b = "Beta")
#' rename_rnd_cor(m, map, rename_cols = TRUE, rnd = 2)
#'
#' # Works if mapping direction is reversed:
#' map_rev <- c("Alpha" = "a", "Beta" = "b")
#' rename_rnd_cor(m, map_rev)
#'
#' @seealso [stats::cor()], [combine_corr_triangles()]
#' @export

rename_rnd_cor <- function(cor_mat,
                           rename_vec = NULL,
                           rename_cols = TRUE,
                           rnd = 2) {
  #cor_matrix = correlation matrix, usually produced by "cor()"
  #rename_vec = a renaming vector that specifies the updated names
  #> Created by doing rename_vec <- c(old_name1 = "New Name 1",
  #old_name2 = "New Name 2")
  #
  #> NOTE: The rename_vec can contain additional variables that I don't have in
  #> the correlation matrix and this function still works.
  if(!is.null(rename_vec)) {
    # Helper: safely rename one side (cols or rows)
    safe_rename <- function(nms, rename_vec) {
      # If mapping looks like new->old (i.e., nms match the values), flip it
      if (!any(nms %in% names(rename_vec)) && any(nms %in% unname(rename_vec))) {
        rename_vec <- setNames(names(rename_vec), unname(rename_vec))  # now old->new
      }
      out <- rename_vec[nms]
      out[is.na(out)] <- nms[is.na(out)]
      out
    }
    #> the safe_rename function takes two arguments:
    #> nms = the colnames of the correlation matrix as "colnames(cor_mat)"
    #> rename_vec = the rename_vec (same as supplied to the original function)

    #> run the safe_rename function here
    colnames(cor_mat) <- safe_rename(colnames(cor_mat), rename_vec)
    rownames(cor_mat) <- safe_rename(rownames(cor_mat), rename_vec)

  }

  if (!is.null(rnd) && rnd != FALSE) {
    cor_mat <- round(cor_mat, rnd)
  }

  #If I want to rename the columns with numbers instead of the full variable
  #names
  if(rename_cols == TRUE) {
    n <- nrow(cor_mat)
    orig_rn <- rownames(cor_mat)
    rownames(cor_mat) <- paste0(orig_rn, " (", seq_len(n), ")")
    colnames(cor_mat) <- as.character(seq_len(n))
  }

  return(cor_mat)
}


#' Combine upper and lower triangles from two correlation matrices
#'
#' Given two square correlation matrices over the same variables, align them to
#' a common order and create a single matrix whose upper triangle comes from the
#' first input and lower triangle from the second input. Control how the
#' diagonal is filled (e.g., use ones for a correlation-style display).
#'
#' @param upper_mat,lower_mat Square numeric matrices with identical variable
#'   sets (names can be in different orders).
#' @param diag_from One of \code{"upper"}, \code{"lower"}, \code{"average"},
#'   \code{"one"}, or \code{"zero"} determining the diagonal values.
#' @param rename_cols Logical; if \code{TRUE}, row names become
#'   \code{"<original> (1..n)"} and column names become \code{"1..n"}.
#'
#' @details
#' Matrices are aligned by column names; both inputs must be square and have
#' non-\code{NULL} dimnames. The result's off-diagonals are taken from the
#' specified triangles of each input.
#'
#' @return A numeric matrix with combined triangles. If \code{rename_cols=TRUE},
#'   returns publication-style numbering in column names and numbered row labels.
#'
#' @examples
#' set.seed(1)
#' x <- cor(matrix(rnorm(60), ncol = 6))
#' y <- cor(matrix(rnorm(60), ncol = 6))
#' dimnames(y) <- dimnames(x) <- list(letters[1:6], letters[1:6])
#' combine_corr_triangles(x, y, diag_from = "one")
#'
#' @seealso [rename_rnd_cor()]
#' @export

combine_corr_triangles <- function(upper_mat, lower_mat,
                                   diag_from = c("upper", "lower", "average", "one", "zero"),
                                   rename_cols = TRUE) {
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
  if(rename_cols == TRUE) {
    orig_rn <- rownames(out)
    rownames(out) <- paste0(orig_rn, " (", seq_len(n), ")")
    colnames(out) <- as.character(seq_len(n))
  }

  return(out)
}



#' Publish-ready two-group correlation display (upper vs. lower triangle)
#'
#' Splits a data frame by a grouping variable, computes correlations on a set of
#' variables for two specified groups, renames/rounds them consistently, and
#' combines them into a single matrix with group 1 in the upper triangle and
#' group 2 in the lower triangle.
#'
#' @param data A data frame.
#' @param cor_vars Character vector of variable names to include in the
#'   correlation matrices.
#' @param group_var Grouping variable; can be supplied bare (unquoted) or as a
#'   string.
#' @param groups Length-2 character vector giving the two group values
#'   (order matters: \code{groups[1]} → upper triangle, \code{groups[2]} → lower).
#' @param rename_vec Optional named character vector for renaming variables
#'   (robust to old→new vs. new→old mapping); see [rename_rnd_cor()].
#' @param rename_cols Logical; if \code{TRUE}, row names become
#'   \code{"<original> (1..n)"} and column names become \code{"1..n"}.
#' @param diag_from How to fill the diagonal in the combined matrix; passed to
#'   [combine_corr_triangles()] (default \code{"one"}).
#' @param cor_use Passed to [stats::cor()] \code{use=} argument (e.g.,
#'   \code{"pairwise"} or \code{"complete"}).
#' @param rnd Integer number of decimals to round to before combining.
#'
#' @details
#' Internally calls \code{compute_split_cors(data, cor_vars, group_var, groups,
#' cor_use)} (not exported here) to produce two correlation matrices. Each is
#' passed through [rename_rnd_cor()] with \code{rename_cols = FALSE} to ensure
#' consistent naming prior to [combine_corr_triangles()].
#'
#' The returned object has class \code{"pub_cors_mat"} and a \code{"note"}
#' attribute describing which group appears in the upper and lower triangles.
#'
#' @return A matrix of class \code{"pub_cors_mat"} with attribute
#'   \code{note = "Upper triangle = <g1>, Lower triangle = <g2>"}.
#'
#' @examples
#' # Toy example
#' set.seed(1)
#' df <- data.frame(
#'   grp = rep(c("Study 1","Study 2"), each = 50),
#'   A = rnorm(100), B = rnorm(100), C = rnorm(100)
#' )
#' pub_cors_by(
#'   data = df,
#'   cor_vars = c("A","B","C"),
#'   group_var = grp,
#'   groups = c("Study 1","Study 2"),
#'   rename_vec = c(A = "Alpha", B = "Beta", C = "Gamma"),
#'   rnd = 2
#' )
#'
#' @seealso [stats::cor()], [rename_rnd_cor()], [combine_corr_triangles()]
#' @export


pub_cors_by <- function(data,
                        cor_vars,
                        group_var,
                        groups,
                        rename_vec = NULL,
                        rename_cols = TRUE,
                        diag_from = "one",
                        cor_use = "pairwise",
                        rnd = 2) {
  #> base r version of allowing bare name or string as input for group_var
  expr <- substitute(group_var)
  gv <- if (is.symbol(expr)) deparse(expr) else as.character(group_var)[1L]

  cor_mats <- compute_split_cors(data, cor_vars, as.name(gv), groups, cor_use = cor_use)


  cor1 <- rename_rnd_cor(cor_mats[[1]],
                         rename_vec = rename_vec,
                         rename_cols = FALSE,
                         rnd = rnd)
  cor2 <- rename_rnd_cor(cor_mats[[2]],
                         rename_vec = rename_vec,
                         rename_cols = FALSE,
                         rnd = rnd)

  out <- combine_corr_triangles(cor1, cor2,
                                diag_from = diag_from,
                                rename_cols = rename_cols)

  #> Adding a custom note to the matrix so I can see what group is upper/lower
  attr(out, "note") <- paste0("Upper triangle = ", groups[1],
                              ", Lower triangle = ", groups[2])
  class(out) <- c("pub_cors_mat", class(out))  # add custom class


  return(out)

}

#' Single-group (pooled) correlation matrix with publication labels
#'
#' Compute a correlation matrix over \code{cor_vars}, optionally rename and
#' round it, and optionally add publication-style numbering.
#'
#' @param data A data frame.
#' @param cor_vars Character vector of variable names to include.
#' @param rename_vec Optional named character vector for renaming variables
#'   (robust to old→new vs. new→old mapping); see [rename_rnd_cor()].
#' @param rename_cols Logical; if \code{TRUE}, row names become
#'   \code{"<original> (1..n)"} and column names become \code{"1..n"}.
#' @param cor_use Passed to [stats::cor()] \code{use=} argument.
#' @param rnd Integer number of decimals to round to. Set \code{FALSE} to skip.
#'
#' @return A correlation matrix (numeric) with possibly renamed and numbered
#'   dimnames suitable for publication.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(A = rnorm(40), B = rnorm(40), C = rnorm(40))
#' pub_cors_all(df, c("A","B","C"),
#'              rename_vec = c(A = "Alpha", B = "Beta", C = "Gamma"),
#'              rename_cols = TRUE, rnd = 2)
#'
#' @seealso [stats::cor()], [rename_rnd_cor()]
#' @export

pub_cors_all <- function(data,
                         cor_vars,
                         rename_vec = NULL,
                         rename_cols = TRUE,
                         cor_use = "pairwise",
                         rnd = 2) {

  cor_mat <- cor(data[, cor_vars], use = cor_use)

  cor_all <- rename_rnd_cor(cor_mat,
                            rename_vec = rename_vec,
                            rename_cols = rename_cols,
                            rnd = rnd)

  return(cor_all)

}

