utils::globalVariables(c("everything", "variables"))



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
#' cor_matrix <- cor(mtcars)
#' cor_format(cor_matrix, bold_val = 0.3)
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
