utils::globalVariables(c("as_string", ":=", "ensym", "group_by", "sym", "ungroup"))


#' Group-Mean Centering for Multi-Level Modeling
#'
#' This function performs group-mean centering for multi-level modeling analyses.
#' It calculates the between-group mean, within-group deviation, and their
#' standardized versions for a specified variable within a given grouping structure.
#' The input data frame must be in long format for accurate calculations.
#' This function is particularly useful for creating multi-level modeling (MLM)
#' predictors and can be applied to multiple variables using a loop or `purrr` workflow.
#'
#' @param df A data frame in long format, containing the data to be processed.
#' @param group The grouping variable (quoted or unquoted) used to define groups.
#' @param variable The target variable (quoted or unquoted) to calculate group means and deviations.
#' @return A data frame with additional columns:
#'   - `BF_<variable>`: Between-group mean of the target variable.
#'   - `ZBF_<variable>`: Standardized between-group mean of the target variable.
#'   - `WF_<variable>`: Within-group deviation of the target variable.
#'   - `ZWF_<variable>`: Standardized within-group deviation of the target variable.

mlm_groupmean <- function(df, group, variable) {

  # Capture variable name as a symbol and string
  variable_sym <- ensym(variable) #useful for passing unqouted variables in tidyverse
  variable_str <- as_string(variable_sym) #useful for paste

  # Define dynamically named columns
  Bvariable <- paste0("BG_", variable_str)   # Between-group mean
  ZBvariable <- paste0("ZBG_", variable_str) # Standardized between-group mean
  Wvariable <- paste0("WG_", variable_str)   # Within-group deviation
  ZWvariable <- paste0("ZWG_", variable_str) # Standardized within-group deviation

  # Perform the calculations
  df1 <- df |>
    group_by({{ group }}) |>
    mutate(
      # Calculate group mean
      !!Bvariable := mean(!!variable_sym, na.rm = TRUE)
    ) |>
    ungroup()  |>
    mutate(
      # Standardize between-group mean
      !!ZBvariable := as.vector(scale(!!sym(Bvariable))),
      # Calculate within-group deviation
      !!Wvariable := !!variable_sym - !!sym(Bvariable),
      # Standardize within-group deviation
      !!ZWvariable := as.vector(scale(!!sym(Wvariable)))
    )

  return(df1)
}
