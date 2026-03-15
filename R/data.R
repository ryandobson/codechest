
#> This is how data files are exported so they are available for use when the
#> package is loaded.

#' @name ryanhonorthesis
#' @docType data
#' @title Ryan Honors Thesis Dataset
#' @format Data are in wide format where the scenario people are responding to emotions
#' on is depicted by "osf." ssf." or "rom." as a variable prefix.
#' \describe{
#'   \item{osf.surprised}{Surprise at opposite-sex friend hanging out with their opposite-sex friend}
#'   \item{ssf.angry}{Anger at opposite-sex friend hanging out with their same-sex friend}
#'   \item{rom.happy}{Happiness at opposite-sex friend hanging out with their romantic parnter}
#' }
#' @source Data collect in fall 2022 for Ryan Dobson's undergrad honor thesis on jealousy in opposite-sex friendships
#' @examples
#' data(ryanhonorthesis)
#' summary(ryanhonorthesis)
"ryanhonorthesis"


#' Five Love Languages Dataset
#'
#' A cleaned dataset derived from my master's thesis examining the
#' psychometric structure of the Five Love Languages.
#'
#' The dataset includes demographic variables, relationship variables,
#' and items measuring both the *receiving* and *expressing* dimensions
#' of the five love languages.
#'
#' @format A data frame with 950 rows and 109 variables.
#' @source Master's thesis dataset collected in summer/fall 2024 and
#' cleaned for inclusion in the codechest package.
#' @examples
#' data(fivelovelanguages)
#' head(fivelovelanguages)
"fivelovelanguages"
