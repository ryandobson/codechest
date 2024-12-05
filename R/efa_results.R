

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
#' @export

efa_results <- function(fa_df, n_factors) {

  # fa_df = the factor analysis data frame to use.
  # n_factors = the number of factors to extract.

  model_name <- psych::fa(fa_df,
                          nfactors = n_factors,
                          fm = "ml",
                          use = "pairwise")
  # Automatically prints the results of this function

  if (n_factors == 1) {  # Only printing this if there is 1 factor
    extract_info <- fac_extrac_fun(fa_df, 1, model_name, "ml", "promax")
    print(extract_info)
  }

  pm_fun_1 <- function(model_name, n_factors) {

    efa_pm <- model_name$loadings[, 1:n_factors] # Factor loading pattern matrix

    efa_pm <- efa_pm |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      tibble::as_tibble() |>
      dplyr::rename("Pattern Matrix Items" = rowname) |>
      dplyr::mutate(across(where(is.double), ~ round(., 2))) |>
      dplyr::mutate(across(where(is.double), ~ kableExtra::cell_spec(., bold = ifelse(. < -.4 | . > .4, TRUE, FALSE)))) |>
      kableExtra::kbl(format = "html", escape = FALSE) |>
      kableExtra::kable_paper("striped") |>
      as.character()  # Removing this allows for printing within a regular R document

    efa_pm
  }

  pm_result <- pm_fun_1(model_name, n_factors)

  if (n_factors > 1) {  # Print factor correlation matrix for multi-factor solutions
    print(paste("Factor Correlation Matrix for", n_factors, "factor solution (Pattern matrix displayed below)"))
    fac_cors <- model_name$Phi # If oblique rotation requested, interfactor correlation
    fac_cors <- round(fac_cors, digits = 2)
    print(fac_cors)
  }

  communal_fun <- function(model_name) {

    efa_coms <- model_name$communality

    efa_coms <- efa_coms |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      tibble::as_tibble() |>
      dplyr::rename(
        "Variable" = rowname,
        "Communalities" = efa_coms
      ) |>
      dplyr::mutate(across(where(is.double), ~ round(., 2))) |>
      dplyr::mutate(across(where(is.double), ~ kableExtra::cell_spec(., bold = ifelse(. > .4, TRUE, FALSE)))) |>
      kableExtra::kbl(format = "html", escape = FALSE) |>
      kableExtra::kable_paper("striped")

    efa_coms
  }

  nice_coms <- communal_fun(model_name)

  # Grabbing mean and range of communality
  efa_coms <- model_name$communality
  com_mean <- round(mean(efa_coms), 2)
  com_min <- round(min(efa_coms), 2)
  com_max <- round(max(efa_coms), 2)

  sm_fun <- function(model_name, n_factors) {

    efa_sm <- model_name$Structure[, 1:n_factors] # Structure matrix

    efa_sm <- efa_sm |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      tibble::as_tibble() |>
      dplyr::rename("Structure Matrix Items" = rowname) |>
      dplyr::mutate(across(where(is.double), ~ round(., 2))) |>
      dplyr::mutate(across(where(is.double), ~ kableExtra::cell_spec(., bold = ifelse(. < -.4 | . > .4, TRUE, FALSE)))) |>
      kableExtra::kbl(format = "html", escape = FALSE) |>
      kableExtra::kable_paper("striped")

    efa_sm
  }

  sm_result <- sm_fun(model_name, n_factors)

  pm_com_fun <- function(efa_model, nfactors) {

    efa_pm <- efa_model$loadings[, 1:nfactors] # Factor loading pattern matrix
    efa_coms <- efa_model$communality

    efa_coms <- efa_coms |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      tibble::as_tibble() |>
      dplyr::rename(
        "Variable" = rowname,
        "Communalities" = efa_coms
      )

    efa_pm <- efa_pm |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      tibble::as_tibble() |>
      dplyr::rename("Variable" = rowname)

    efa_pm_com <- efa_pm |>
      dplyr::left_join(efa_coms, by = "Variable") |>
      dplyr::mutate(across(where(is.double), ~ round(., 2))) |>
      dplyr::mutate(across(where(is.double), ~ kableExtra::cell_spec(., bold = ifelse(. < -.4 | . > .4, TRUE, FALSE)))) |>
      kableExtra::kbl(format = "html", escape = FALSE) |>
      kableExtra::kable_paper("striped")

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

  return(final) # Returns the efa model to the global environment
}


utils::globalVariables(c("fac_num", "value"))


#' Factor Extraction Results
#'
#' This function extracts and analyzes key results from factor analysis, including eigenvalues, scree plots, and other diagnostics.
#'
#' @param df A data frame containing the data for factor analysis.
#' @param nfactors An integer specifying the number of factors to extract.
#' @param efa_model An object from `psych::fa()` containing the factor analysis model.
#' @param estimation_meth A string specifying the estimation method (e.g., `"ml"` for maximum likelihood).
#' @param rotation_meth A string specifying the rotation method (e.g., `"promax"`).
#'
#' @return A list containing:
#'   \item{Scree Plot}{A `ggplot2` object showing the scree plot of eigenvalues.}
#' @details
#' The function calculates eigenvalues, performs parallel analysis, and generates a scree plot using `ggplot2`.
#' For 1-factor solutions, the function computes a Very Simple Structure (VSS) summary.
#'
#' @examples
#' \dontrun{
#' efa_model <- psych::fa(my_data, nfactors = 3, fm = "ml")
#' results <- fac_extrac_fun(my_data, nfactors = 3, efa_model, "ml", "promax")
#' results$`Scree Plot`
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 ggplot aes geom_point geom_label theme_classic labs coord_cartesian scale_x_continuous scale_y_continuous guides theme element_line element_text
#' @importFrom psych fa.parallel vss
#' @export
fac_extrac_fun <- function(df, nfactors, efa_model, estimation_meth, rotation_meth) {

  # Kaiser Criterion
  eigen_1 <- efa_model$e.values[efa_model$e.values >= 1]  # Eigenvalues >= 1
  eigen_1_length <- length(eigen_1)                      # Count of eigenvalues >= 1

  # Parallel Analysis
  fa_par <- psych::fa.parallel(
    x = df,                 # Data matrix for analysis
    fm = estimation_meth,   # Estimation method
    fa = "fa",              # Factor analysis
    plot = FALSE            # Disable plot
  )
  fa_par_nfact <- fa_par$nfact

  # Scree Plot Data Preparation
  scree_data <- efa_model$e.values |>
    tibble::as_tibble() |>
    dplyr::mutate(
      fac_num = 1:ncol(df),      # Add factor numbers
      value = round(value, 2)    # Round eigenvalues
    )

  # Scree Plot with ggplot2
  scree_plot <- scree_data |>
    ggplot2::ggplot(ggplot2::aes(fac_num, value, label = value)) +
    ggplot2::geom_point() +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = "Scree Plot",
      y = "Eigenvalue",
      x = "Number of Factors"
    ) +
    ggplot2::coord_cartesian(xlim = c(1, 12)) +
    ggplot2::scale_x_continuous(breaks = seq(1, 12, by = 1)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 30, by = 5)) +
    ggplot2::geom_label(size = 6) +
    ggplot2::guides(color = "none") +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "grey", size = 0.5),
      title = ggplot2::element_text(margin = ggplot2::margin(b = 8))
    )

  # Very Simple Structure (VSS) Analysis
  if (nfactors == 1) {
    fa_vss <- psych::vss(df, fm = estimation_meth, plot = FALSE)
  } else {
    fa_vss <- psych::vss(df, rotate = rotation_meth, fm = estimation_meth, plot = FALSE)
  }
  fa_vss_sum <- summary(fa_vss)

  # Final Output
  final <- list(
    "Scree Plot" = scree_plot
  )

  return(final)
}




