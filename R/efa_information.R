
utils::globalVariables(c(".data", "all_of", "filter", "item_name", "pull", "select"))


#' Extract and Format Variance Accounted for in Factor Analysis Model
#'
#' This function takes an EFA (Exploratory Factor Analysis) model object and extracts
#' the variance accounted for by the factors. It processes the variance data by rounding,
#' formatting, and displaying the top rows in a neatly formatted HTML table.
#'
#' @param efa_model A factor analysis model object (typically from `factanal` or similar).
#' The object must contain a `Vaccounted` attribute representing the variance accounted for by the factors.
#'
#' @return A formatted HTML table (as a `kable` object) displaying the variance accounted for
#' by the factors, rounded to two decimal places, and excluding unnecessary rows.
#'
#' @details
#' The `efa_vaccount()` function accesses the `Vaccounted` component of an EFA model, which contains
#' the proportion of variance explained by each factor. It processes the data to:
#' \itemize{
#'   \item Keep only the first three rows to avoid unnecessary information.
#'   \item Round numerical values to two decimal places for clarity.
#'   \item Format the table as a striped HTML table using the `kable` and `kableExtra` package functions.
#' }
#'
#' This function is designed to provide a quick and clean view of the variance explained by factors in an EFA.
#'
#' @importFrom dplyr slice rename mutate across
#' @importFrom kableExtra kbl kable_paper
#' @importFrom tibble rownames_to_column
#' @export
#'
#' @examples
#' # Example usage with a factor analysis model
#' # Assuming `efa_model` is a valid factor analysis model object
#' #efa_vaccount(efa_model)

efa_vaccount <- function(efa_model) {

  efa_model$Vaccounted |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::rename("Information" = rowname) |>
    dplyr::slice(1:3) |>  #removing unecessary proportion of variance accounted for rows
    dplyr::mutate(across(where(is.double), ~ round(., 2))) |>  #rounding for clarity
    kableExtra::kbl(format = "html", escape = FALSE) |>
    kableExtra::kable_paper("striped")

}



#Two related functions in this document
#fac_alphas calculates all of the alphas for each item on a factor that
#loads a specified amount (default loading cutoff = .40)
#> fac_lodaings calculates the number of items loading >.40 on a factor,
#> the mean, min, max factor loadings, and which items load on the factor.

#' Calculate Cronbach's Alpha for Items on Each Factor
#'
#' This function calculates Cronbach's alpha for the items on each factor of an Exploratory Factor Analysis (EFA) model.
#' The function filters items based on a specified loading cutoff (default loading cutoff = 0.40) and then calculates the
#' internal consistency of those items using the Cronbach's alpha statistic. The function also calculates and returns
#' inter-item correlations for each factor.
#'
#' @param df A data frame containing the observed data, with each column representing an item.
#' @param efa_model An EFA model object that contains the factor loadings. This object should have the `loadings`
#' matrix and the number of factors (`factors` attribute).
#' @param load_cut Numeric value for the factor loading cutoff (default = 0.40). This determines which items are
#' considered for each alpha calculation
#' @param all_output Logical. If `TRUE`, the function returns all available output from the `psych::alpha()` function,
#' including detailed item-by-item alpha statistics. If `FALSE` (the default), it returns a summarized result with
#' total alpha and inter-item correlations.
#'
#' @return A list containing results for each factor:
#' \describe{
#'   \item{`Alpha Totals`}{The overall Cronbach's alpha for the items loading on each factor.}
#'   \item{`Alpha Item Information`}{Detailed item-wise alpha statistics, including drop values.}
#'   \item{`Mean Inter-item Correlation`}{The mean correlation between items on the factor.}
#'   \item{`Minimum/Maximum Inter-item Correlation`}{The minimum and maximum inter-item correlations for the factor.}
#' }
#'
#' @details
#' The `fac_alphas` function uses the `psych::alpha()` function to calculate Cronbach's alpha, a measure of internal
#' consistency. The function allows users to filter items based on a factor loading threshold (default = 0.40) and returns
#' either a full output or a summary, depending on the `all_output` argument. The function is particularly useful for
#' assessing the reliability of scales or factors in factor analysis.
#'
#' @importFrom dplyr filter select mutate rename pull mutate_if
#' @importFrom psych alpha
#' @importFrom tibble rownames_to_column
#' @export
#'
#' @examples
#' # Example usage of fac_alphas
#' # Assuming `efa_model` is a valid EFA model object from a factor analysis
#' #fac_alphas(df, efa_model)
fac_alphas <- function(df, efa_model, load_cut = .40, all_output = FALSE)  {

  num_of_factors <- efa_model$factors

  #pattern matrix to use for analyses
  efa_pm <- efa_model$loadings[, 1:num_of_factors] |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::rename(item_name = rowname) |>
    dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
  #getting the factor names (need this done dynamically for different models)
  if (num_of_factors == 1) {
     colnames(efa_pm)[2] <- "Xfactor"
    fac_names <- "Xfactor"
  } else {
    #getting the factor names (need this done dynamically for different models)
    fac_names <- efa_model$loadings[, 1:num_of_factors] |> colnames() #its easiest to grab the names via the loading matrix
  }

  #initialize a list to store alphas
  efa_info <- list()

  for (i in fac_names) {

    variables <- efa_pm |>
      dplyr::filter(.data[[i]] >= load_cut) |>
      dplyr::select(item_name)
    variables <- as.vector(variables$item_name)

    #selecting the desired variables from the data frame
    items <- df |> dplyr::select(all_of(variables))

    item_n <- length(items)

    cron_alpha <- psych::alpha(items[, 1:item_n]) #alpha of items
    factor_alpha_name <- paste(i, sep = "_")


    #> If I want all of the output I need to specify it as "TRUE" in the function
    #> call.
    #> else it will just print a few abreviated things that are good in most cases.
    if (all_output) {

      efa_info[[factor_alpha_name]] <- cron_alpha

    } else {
      efa_info[[factor_alpha_name]] <- list(
        "Alpha Totals:" = cron_alpha$total,
        "Alpha Item Information:" = cron_alpha$alpha.drop,
        "Mean inter-item correlation:" =  round(mean(cron_alpha$alpha.drop[["average_r"]]), 2),
        "Minimum inter-item correlation:" = round(min(cron_alpha$alpha.drop[["average_r"]]), 2),
        "Maximum inter-item correlation:" = round(max(cron_alpha$alpha.drop[["average_r"]]), 2)

      )
    }
  }
  return(efa_info)

}



#' Calculate Number of Items Loading on Each Factor and Factor Loading Statistics
#'
#' This function calculates the number of items loading on each factor based on a specified cutoff (default = 0.40).
#' It also computes summary statistics for the factor loadings, including the mean, minimum, and maximum factor loadings
#' for each factor. The function returns the number of items, the items themselves, and loading statistics for each factor.
#'
#' @param df A data frame containing the observed data, with each column representing an item.
#' @param efa_model An EFA model object that contains the factor loadings. This object should have the `loadings`
#' matrix and the number of factors (`factors` attribute).
#' @param load_cut Numeric value for the factor loading cutoff (default = 0.40). This determines which items are
#' considered as loading on the factor.
#'
#' @return A list containing results for each factor:
#' \describe{
#'   \item{`Number of Items`}{The number of items loading on the factor above the specified cutoff.}
#'   \item{`Items`}{The names of the items that load on the factor above the cutoff.}
#'   \item{`Factor Loading Mean`}{The mean of the factor loadings for the factor.}
#'   \item{`Factor Loading Min/Max`}{The minimum and maximum factor loadings for the factor.}
#' }
#'
#' @details
#' The `fac_loadings` function filters items based on their factor loadings above a specified cutoff (default = 0.40).
#' It provides summary statistics for these loadings, including the number of items, as well as the mean, minimum, and
#' maximum loadings for each factor. This function is useful for understanding which items contribute most to each factor.
#'
#' @importFrom dplyr filter select mutate rename pull mutate_if
#' @importFrom tibble rownames_to_column
#' @export
#'
#' @examples
#' # Example usage of fac_loadings
#' # Assuming `efa_model` is a valid EFA model object from a factor analysis
#' #fac_loadings(df, efa_model)
fac_loadings <- function(df, efa_model, load_cut = .40)  {

  num_of_factors <- efa_model$factors

  #pattern matrix to use for analyses
  efa_pm <- efa_model$loadings[, 1:num_of_factors] |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::rename(item_name = rowname) |>
    dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
  #getting the factor names (need this done dynamically for different models)
  if (num_of_factors == 1) {
    colnames(efa_pm)[2] <- "Xfactor"
    fac_names <- "Xfactor"
  } else {
    #getting the factor names (need this done dynamically for different models)
    fac_names <- efa_model$loadings[, 1:num_of_factors] |> colnames() #its easiest to grab the names via the loading matrix
  }

  #initialize a list to store alphas
  efa_info <- list()

  for (i in fac_names) {

    factor_loadings <- efa_pm |>
      dplyr::filter(.data[[i]] > load_cut) |>
      dplyr::pull(i)

    factor_items <- efa_pm |>
      dplyr::filter(.data[[i]] >= load_cut) |>
      dplyr::pull(item_name)

    num_of_items <- length(factor_loadings)
    fl_min <- round(min(factor_loadings), 2)
    fl_max <- round(max(factor_loadings), 2)
    fl_avg <- round(mean(factor_loadings), 2)

    factor_name <- paste(i, sep = "_")

    efa_info[[factor_name]] <- list(
      "Number of items loading > .40 (or specified cutoff):" = num_of_items,
      "Items:" = factor_items,
      "Factor Loading Mean:" = fl_avg,
      "Factor Loading Min:" = fl_min,
      "Factor Loading Max:" = fl_max

    )

  }
  return(efa_info)

}


