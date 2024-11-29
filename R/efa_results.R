

utils::globalVariables(c("as_tibble", "fa", "fac_extrac_fun", "kable_paper",
                         "left_join", "mutate", "rename", "rownames_to_column",
                         "kbl"))


#' Perform and Summarize Exploratory Factor Analysis
#'
#' This function conducts exploratory factor analysis (EFA) using a maximum likelihood
#' estimator and summarizes the results, including the pattern matrix, communalities,
#' and other relevant outputs.
#'
#' @param fa_df A data frame containing the data for factor analysis.
#' @param n_factors An integer specifying the number of factors to extract.
#'
#' @return A list containing:
#'   \item{efa}{The EFA model object.}
#'   \item{pm_com}{A combined table of the pattern matrix and communalities formatted for display.}
#'   \item{pm}{The formatted pattern matrix.}
#'   \item{sm}{The formatted structure matrix.}
#'   \item{com_mean}{The mean of communalities.}
#'   \item{com_min}{The minimum communality value.}
#'   \item{com_max}{The maximum communality value.}
#'   \item{coms}{The formatted communalities table.}
#'
#' @details
#' If the number of factors (`n_factors`) is 1, additional information about the single-factor
#' solution is printed. For multi-factor solutions, the function also prints the factor
#' correlation matrix (if oblique rotation is used).
#'
#' @examples
#' # Example usage:
#' # efa_results(my_data, 3)
#'
#' @import dplyr
#' @import tibble
#' @import kableExtra
#' @importFrom psych fa
#' @importFrom dplyr rownames_to_column
#' @export

efa_results <- function(fa_df, n_factors) {

  #fa_df = the factor analysis data frame to use.
  #model_name = the newly created name for your model

  model_name <- fa(fa_df,
                   nfactors = n_factors,
                   fm = "ml",
                   use = "pairwise")
  #this automatically prints the results of this function

  if (n_factors == 1) {  #> Only printing this if there is 1 factor
    extract_info <- fac_extrac_fun(fa_df, 1, model_name, "ml", "promax")
    print(extract_info)
  }


  pm_fun_1 <- function(model_name, n_factors) {

    efa_pm <- model_name$loadings[, 1:n_factors] #Factor loading pattern matrix; adjust columns to number of factors

    efa_pm <- efa_pm |>
      as.data.frame()|>
      rownames_to_column() |>
      as_tibble() |>
      rename("Pattern Matrix Items" = rowname) |>
      #mutate(across(where(is.double), ~ ifelse(. < .15 & . > -.15, NA, .))) |> #cut low
      mutate(across(where(is.double), ~ round(., 2))) |> #round
      mutate(across(where(is.double), ~ cell_spec(., bold = ifelse(. < -.4 | . > .4, TRUE, FALSE)))) |>
      #mutate(across(everything(), as.character)) |> #character for NA
      #mutate(across(everything(), ~ replace_na(., "")))  |> #blank spaces
      kbl(format = "html", escape = FALSE) |>
      kable_paper("striped") |>
      as.character()  #removing this allows for printing within regular r document

    efa_pm

  }

  pm_result <- pm_fun_1(model_name, n_factors)


  #> When I have more than 1 factor I also want to print the factor correlation matrix:
  if (n_factors > 1) {
    print(paste("Factor Correlation Matrix for", n_factors, "factor solution (Pattern matrix displayed below)"))
    #print(model_name$score.cor, digits = 3)#Factor correlation of latent variable factor scores
    fac_cors <- model_name$Phi #If oblique rotation requested, what is the interfactor correlation?
    fac_cors <- (model_name$Phi) |> round(digits = 2) #round factor scores

    # Hide upper triangle
    #fac_cors[upper.tri(fac_cors, diag = TRUE)] <- ""
    #basically just selecting the upper part of the triangle and replacing it with nothing
    #Although, it changes it to a character object now.
    #fac_cors <- as_tibble(fac_cors) |> mutate_all(as.double)

    print(fac_cors)

  }

  communal_fun <- function(model_name) {

    efa_coms <- model_name$communality

    efa_coms <- efa_coms |>
      as.data.frame()|>
      rownames_to_column() |>
      as_tibble() |>
      rename(
        "Variable" = rowname,
        "Communalities" = efa_coms
      ) |>
      mutate(across(where(is.double), ~ round(., 2))) |> #round
      mutate(across(where(is.double), ~ cell_spec(., bold = ifelse(. > .4, TRUE, FALSE)))) |>
      kbl(format = "html", escape = FALSE) |>
      kable_paper("striped")

    efa_coms

  }

  nice_coms <- communal_fun(model_name)

  #> Grabbing mean and range of communality
  efa_coms <- model_name$communality
  com_mean <- round(mean(efa_coms), 2)
  com_min <- round(min(efa_coms), 2)
  com_max <- round(max(efa_coms), 2)


  sm_fun <- function(model_name, n_factors) {

    efa_sm <- model_name$Structure[, 1:n_factors] #Factor loading pattern matrix; adjust columns to number of factors

    efa_sm <- efa_sm |>
      as.data.frame()|>
      rownames_to_column() |>
      as_tibble() |>
      rename("Structure Matrix Items" = rowname) |>
      #mutate(across(where(is.double), ~ ifelse(. < .15 & . > -.15, NA, .))) |> #cut low
      mutate(across(where(is.double), ~ round(., 2))) |> #round
      mutate(across(where(is.double), ~ cell_spec(., bold = ifelse(. < -.4 | . > .4, TRUE, FALSE)))) |>
      #mutate(across(everything(), as.character)) |> #character for NA
      #mutate(across(everything(), ~ replace_na(., "")))  |> #blank spaces
      kbl(format = "html", escape = FALSE) |>
      kable_paper("striped")

    efa_sm

  }

  sm_result <- sm_fun(model_name, n_factors)


  pm_com_fun <- function(efa_model, nfactors) {

    efa_pm <- efa_model$loadings[, 1:nfactors] #Factor loading pattern matrix; adjust columns to number of factors

    efa_coms <- efa_model$communality #NOTE: there are also "communalities"
    #available to be extracted but these are reflecting common variance.
    #I'm pretty sure for most cases I want "communality" which ar ethe sum of
    #squared factor loadings for that item.

    efa_coms <- efa_coms |>
      as.data.frame()|>
      rownames_to_column() |>
      as_tibble() |>
      rename(
        "Variable" = rowname,
        "Communalities" = efa_coms
      )

    efa_pm <- efa_pm |>
      as.data.frame()|>
      rownames_to_column() |>
      as_tibble() |>
      rename(
        "Variable" = rowname
      )

    efa_pm_com  <- efa_pm |> left_join(efa_coms, by = "Variable") |>
      #mutate(across(where(is.double), ~ ifelse(. < .15 & . > -.15, NA, .))) |> #cut low
      mutate(across(where(is.double), ~ round(., 2))) |> #round
      mutate(across(where(is.double), ~ cell_spec(., bold = ifelse(. < -.4 | . > .4, TRUE, FALSE)))) |>
      #mutate(across(everything(), as.character)) |> #character for NA
      #mutate(across(everything(), ~ replace_na(., "")))  |> #blank spaces
      kbl(format = "html", escape = FALSE) |>
      kable_paper("striped")

    efa_pm_com

  }

  efa_pm_com_result <- pm_com_fun(model_name, n_factors)

  final <- list(
    "efa" = model_name,
    "pm_com" = efa_pm_com_result,
    "pm" = pm_result,
    "sm" = sm_result,
    "com_mean" = com_mean,
    "com_min" = com_min,
    "com_max" = com_max,
    "coms" = nice_coms
  )

  return(final) #this returns the efa model to the global environment
}






