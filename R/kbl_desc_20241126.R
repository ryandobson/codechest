#' Create a Descriptive Summary Table (Version 1)
#'
#' This function generates a descriptive summary table for a data frame, including
#' rounded numeric values and highlighting skewness and kurtosis values that exceed thresholds.
#'
#' @param df A data frame containing numeric variables.
#' @return A styled HTML table summarizing the descriptive statistics.
#' @examples
#' \dontrun{
#' library(psych)
#' library(dplyr)
#' library(kableExtra)
#' library(tibble)
#' }
#' @export
kbl_descV1 <- function(df) {
  kbl_desc <- psych::describe(df) |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::select(-vars, -mad, -median) |>
    dplyr::mutate(across(where(is.double), ~ round(.,  2))) |>
    dplyr::rename(variable = rowname) |>
    dplyr::mutate(
      skew = kableExtra::cell_spec(skew, bold = ifelse(skew > 2 | skew < -2, TRUE, FALSE)),
      kurtosis = kableExtra::cell_spec(kurtosis, bold = ifelse(kurtosis > 3 | kurtosis < -3, TRUE, FALSE))
    ) |>
    kableExtra::kbl(format = "html", escape = FALSE) |>
    kableExtra::kable_paper("striped", full_width = FALSE)

  return(kbl_desc)
}




#' Create a Descriptive Summary Table with Dependency Checks (Version 2)
#'
#' This function generates a descriptive summary table for a data frame, including
#' rounded numeric values and highlighting skewness and kurtosis values that exceed thresholds.
#' It checks for required packages and loads them dynamically.
#'
#' @param df A data frame containing numeric variables.
#' @return A styled HTML table summarizing the descriptive statistics.
#' @examples
#' \dontrun{
#' library(psych)
#' library(dplyr)
#' library(kableExtra)
#' library(tibble)
#' }
#' @export
kbl_descV2 <- function(df) {
  required_packages <- c("psych", "dplyr", "kableExtra", "tibble")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "is required but not installed. Please install it."))
    }
    if (!pkg %in% .packages()) {
      library(pkg, character.only = TRUE)
    }
  }

  kbl_desc <- psych::describe(df) |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::select(-vars, -mad, -median) |>
    dplyr::mutate(across(where(is.double), ~ round(.,  2))) |>
    dplyr::rename(variable = rowname) |>
    dplyr::mutate(
      skew = kableExtra::cell_spec(skew, bold = ifelse(skew > 2 | skew < -2, TRUE, FALSE)),
      kurtosis = kableExtra::cell_spec(kurtosis, bold = ifelse(kurtosis > 3 | kurtosis < -3, TRUE, FALSE))
    ) |>
    kableExtra::kbl(format = "html", escape = FALSE) |>
    kableExtra::kable_paper("striped", full_width = FALSE)

  return(kbl_desc)
}

