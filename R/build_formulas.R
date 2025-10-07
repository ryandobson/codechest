
utils::globalVariables(c("reformulate"))



#' Build Random Effects Term for Mixed Models
#'
#' Constructs a properly formatted random effects term for use in mixed-effects model formulas.
#' Automatically checks variable existence (if data provided), adds intercepts if missing,
#' and allows specification of correlated (`|`) or uncorrelated (`||`) random slopes.
#'
#' @param random_effects Character vector of random effect variable names.
#'   Can include `"1"` to specify a random intercept; if not present and `intercept = TRUE`,
#'   one will be added automatically.
#' @param group Character scalar giving the grouping variable for the random effects
#'   (e.g., `"person_id"`, `"family_id"`).
#' @param covary Logical; whether to model correlated (`TRUE`) or uncorrelated (`FALSE`)
#'   random slopes and intercepts.
#' @param intercept Logical; if `TRUE`, automatically add a random intercept when not specified.
#' @param data Optional data frame. If supplied, variable existence will be checked.
#'
#' @return A character string representing the random effects portion of a mixed model formula,
#'   e.g. `"(1 + slope | group)"` or `"(1 + slope || group)"`.
#'
#' @examples
#' build_random_term(c("1", "x"), group = "id")
#' build_random_term("x", group = "id", covary = FALSE)
#'
#' @export
build_random_term <- function(random_effects,
                              group,
                              covary = TRUE,
                              intercept = TRUE,
                              data = NULL) {

  #> This function is highly useful for creating my random effect terms

  #> random_effects = a vector with all of the random slope terms to include in the
  #> model. The random intercept can also be specified here (e.g., "1"). However,
  #> if it is not specified, it will automatically be added in.
  #> group = the grouping variable for the random slopes/intercept (e.g., person_id
  #> family_id, etc.)
  #> covary = TRUE -- automatically assumes I want correlated random intercept/slope
  #> intercept = TRUE -- automatically assume I want to add in a random intercept
  #> if the random_effects term does not include it.
  #> data = the dataframe on which the model is going to be fit. Only need to include
  #> if I want to check if the variables exist.

  # type checks
  if (!is.character(random_effects)) {
    stop("Error in build_random_term(): `random_effects` must be a character vector of variable names")
  }
  if (!is.character(group) || length(group) != 1) {
    stop("Error in build_random_term(): `grouping_variable` must be a single character string")
  }
  if (!is.logical(covary) || length(covary) != 1) {
    stop("Error in build_random_term(): `covary` must be a single TRUE/FALSE value")
  }


  # optional dataset check
  if (!is.null(data)) {

    #create a character vector of variables in random effects (besides "1") which
    #are not in the relevant dataset.
    missing_vars <- setdiff(random_effects[random_effects != "1"], names(data))
    if (!length(missing_vars) == 0) {
      stop("These random effect terms are not in the dataset: ",
           paste(missing_vars, collapse = ", "))
    }

    #check to see if my grouping variable exists in the dataset.
    if (!(group %in% names(data))) {
      stop("Grouping variable '", group, "' is not in the dataset")
    }
  }


  # If no random intercept included, automatically add it in
  if (intercept == TRUE && !any(random_effects == "1")) {

    warning("No random intercept term '1' was provided and was automatically
  included. If no random intercept is desired specify `intercept = FALSE`.
            IGNORE IF RANDOM INTERCEPT IS DESIRED")
    random_effects <- c(1, random_effects) #numeric gets coerced to character so
    #I don't need to quote it here.
  }

  # if intercept-only, short-circuit
  if (identical(random_effects, "1")) {
    return(paste0("(1 | ", group, ")"))
  }

  #switch for using correlated or uncorrelated random slopes
  covar_term <- switch(as.character(covary),
                       "TRUE" = "|",
                       "FALSE" = "||")

  if(length(random_effects) == 1 & covary == TRUE & intercept == FALSE) {

    warning("You specified single random slope and no intercept, but also specfied
            that you want correlated randome effects. Your model will automatically
            input the intercept term.
            If you want a single random slope and no intercept, please specify
            `covary == FALSE`")

  }

  #creates a string of: "(1 + random effect + random effect + ... random effect || id1)"
  random_part <- paste("(", paste0(paste(random_effects, collapse = " + "), covar_term, group, ")"), sep = "")

  return(random_part)

}


#' Build Fixed Effects Term for Mixed Models
#'
#' Constructs a string of fixed effects to be used in a model formula.
#' Optionally checks whether variables exist in a supplied dataset and excludes
#' interaction terms from the check.
#'
#' @param fixed_effects Character vector of fixed effect variable names.
#'   Can include interaction terms (e.g., `"x1*x2"` or `"x1:x2"`).
#' @param data Optional data frame. If supplied, function verifies that
#'   the variables (excluding interaction terms) exist in the dataset.
#'
#' @return A single character string joining all fixed effect terms with `" + "`.
#'
#' @examples
#' \dontrun{ #these examples fail to run, but they are good enough examples to
#' #provide a general example.
#' build_fixed_term(c("x1", "x2", "x1:x2"))
#' build_fixed_term(c("x1", "x2"), data = mtcars)
#' }
#' @export
build_fixed_term <- function(fixed_effects, data = NULL) {

  #> fixed_effects = a character vector of terms to include in the model. This
  #> will include interaction terms (so I will have to filter those out below).
  #> data = NULL -- if I want to check to see if my variables exist in my dataset
  #> I can include the relvant df and then it will automatically check for the
  #> existence of the variables in fixed_effects in my df.

  # type checks
  if (!is.character(fixed_effects)) {
    stop("Error in build_fixed_term(): `fixed_effects` must be a character vector of variable names")
  }

  #if a dataframe is supplied, check to see if the fixed effects provided exist
  #in that dataframe.
  if (!is.null(data)) {
    #filter out any interaction terms within the fixed_effects
    check_vars <- fixed_effects[!grepl("\\*|:", fixed_effects)]
    #"\\*" the backslashes are needed to escape the "*" operator.

    missing_vars <- setdiff(check_vars, names(data))
    if (!length(missing_vars) == 0) {
      stop(paste("Error in build_fixed_term():",missing_vars, "are not in the
                  dataset"))
    }

  }

  fixed_part <- paste(fixed_effects, collapse = " + ")

  return(fixed_part)
}



#' Build Full Model Formula for Mixed Models
#'
#' Combines dependent variable, fixed effects, and optional random effects
#' into a single model formula object. Designed to integrate with
#' `lmer()`, `glmer()`, or similar mixed model functions.
#' Automatically checks variable existence and environment scoping.
#'
#' @param dv Character scalar; name of the dependent variable.
#' @param fixed_effects Character vector of fixed effect variable names
#'   (can include interactions).
#' @param random_effects Optional character vector of random effect variable names.
#'   If omitted, the model will include only fixed effects.
#' @param group Character scalar specifying the grouping variable for random effects.
#'   Required if `random_effects` are specified.
#' @param model_env Optional environment to which the formula will be bound.
#'   Defaults to a new environment whose parent is the global environment.
#' @param covary Logical; if `TRUE`, random slopes and intercepts are modeled as correlated.
#' @param intercept Logical; if `TRUE`, automatically add a random intercept if missing.
#' @param data Optional data frame. If supplied, checks that all variables exist.
#'
#' @return A `formula` object suitable for model fitting.
#'
#' @examples
#' \dontrun{#these examples fail to run, but they are good enough examples to
#' #provide a general example.
#' build_formula("y", c("x1", "x2"), random_effects = c("1", "x1"), group = "id")
#' build_formula("mpg", "wt", data = mtcars)
#' }
#' @export
build_formula <- function(dv,
                          fixed_effects,
                          random_effects = NULL, #should always be provided,
                          #but there are cases in mlm_compare function where it
                          #might not be.
                          group = NULL,
                          model_env = NULL, #the environment to save the formula to
                          covary = TRUE, #automatically assume correlated random parts
                          intercept = TRUE, #automatically add in intercept if missing
                          data = NULL) { #relevant df to check for variable existence
  #> if an environment is not provided, create a new one that uses the global
  #> environment as its parent.
  #> This resolves scoping issues involved with the formulas created within functions
  #> not being able to find data
  if(is.null(model_env)) {model_env <- new.env(parent = globalenv())}
  #> NOTE: ChatGPT suggested using parent.frame() instead of the globalenv.
  #> parent.frame() stores the formula in the environment from which the function
  #> was called.
  #> My hunch is that this will cause scoping issues, but I'm not sure at this
  #> point in time.


  #> check if dependent variable exists in the data
  if (!is.null(data)) {
    dv_missing <- setdiff(dv, names(data))
    #if dv_missing is not a length of zero (i.e., missing variable was identified)
    #then stop
    if(!length(dv_missing) == 0) stop(paste("Error in build_formula(): The dv --", dv_missing, "--is not in the
                  dataset"))
  }

  #call build_fixed_term function to properly build out fixed effects
  fixed_part <- build_fixed_term(fixed_effects = fixed_effects, data = data)

  #If no random terms are supplied, just used the fixed effects
  no_random <- is.null(random_effects) || length(random_effects) == 0 ||
    (length(random_effects) == 1 && identical(random_effects, ""))
  if (no_random) {
    term <- fixed_part
  } else {
    random_part <- build_random_term(random_effects = random_effects,
                                     group = group,
                                     covary = covary,
                                     intercept = intercept,
                                     data = data)

    term <- paste(fixed_part, random_part, sep = " + ")
  }


  formula <- reformulate(termlabels = term, response = dv,
                         env = model_env)
  return(formula)
}

#' Build Formulas for a List of Models
#'
#' Uses \code{\link{build_formula}} (and helpers) to construct model formulas
#' for each entry in a supplied \code{model_list}. For each model, the function
#' verifies required inputs, optionally checks variable existence in the
#' associated dataset, and attaches a \code{$formula} element to that model's
#' list entry. Models with insufficient information are skipped with a warning.
#'
#' @param model_list A named list where each element is itself a list describing
#'   a model. Each element should include:
#'   \itemize{
#'     \item \code{dependent_variable} Character scalar; the outcome variable name.
#'     \item \code{fixed_effects} Character vector; fixed-effect terms (may include interactions).
#'     \item \code{random_effects} Optional character vector; random-effect terms (may include "1").
#'     \item \code{grouping_variable} Optional character scalar; grouping variable for random effects.
#'     \item \code{data} Character scalar; the name of a data frame object (looked up via \code{get()}).
#'     \item \code{name} Optional character scalar; used only for clearer warning messages.
#'   }
#' @param covary Logical; if \code{TRUE}, random slopes/intercepts are modeled as correlated
#'   (uses \code{|}); if \code{FALSE}, uncorrelated (uses \code{||}). Passed to \code{build_random_term()}.
#' @param intercept Logical; if \code{TRUE}, a random intercept is added if not explicitly included
#'   in \code{random_effects}. Passed to \code{build_random_term()}.
#' @param model_env Optional environment in which the resulting \code{formula} objects will be bound.
#'   If \code{NULL}, a new environment is created with \code{globalenv()} as parent (a warning is issued).
#' @param ... Reserved for future expansion; currently unused.
#'
#' @details
#' For each model entry, the function:
#' \enumerate{
#'   \item Retrieves the data frame via \code{get(model_list[[i]]$data)}.
#'   \item Calls \code{\link{build_formula}} with the provided components.
#'   \item Attaches the resulting \code{formula} as \code{model_list[[i]]$formula}.
#' }
#' Models are skipped (with warnings) if:
#' \itemize{
#'   \item \code{data} is \code{NULL} or not found,
#'   \item \code{random_effects} is provided but \code{grouping_variable} is missing,
#'   \item \code{grouping_variable} is provided but \code{random_effects} is missing.
#' }
#'
#' @return The input \code{model_list}, with a \code{$formula} element added to each
#'   model successfully processed. Entries that were skipped remain unchanged.
#'
#' @examples
#' \dontrun{
#' ml <- list(
#'   m1 = list(
#'     name = "Model 1",
#'     dependent_variable = "y",
#'     fixed_effects = c("x1", "x2", "x1:x2"),
#'     random_effects = c("1", "x1"),
#'     grouping_variable = "id",
#'     data = "df"   # name of a data.frame in the environment
#'   ),
#'   m2 = list(
#'     name = "Model 2 (fixed only)",
#'     dependent_variable = "y",
#'     fixed_effects = c("x1", "x2"),
#'     data = "df"
#'   )
#' )
#'
#' # Suppose df exists in the environment
#' out <- build_formulas(ml, covary = TRUE, intercept = TRUE)
#' out$m1$formula
#' out$m2$formula
#' }
#'
#' @seealso \code{\link{build_formula}}, \code{\link{build_random_term}}, \code{\link{build_fixed_term}}
#'
#' @export
build_formulas <- function(model_list,
                           covary = TRUE,
                           intercept = TRUE,
                           model_env = NULL,
                           ...) {

  #> model_list is a list object of all of the model information needed to
  #> specify the formulas
  #> Each model within model_list must have all of the elements needed to specify
  #> build_formula, which are as follows:
  # dv -- the dependent variable
  # fixed_effects -- character vector of fixed effects
  # random_effects = NULL -- if I am using a regular lm, I don't need to supply
  #this character #vector
  # group -- if I have random effects, I need the grouping variable (e.g., family_id, pid)
  # model_env = NULL, #the environment to save the formula to
  # covary = TRUE, #automatically assume correlated random parts
  # intercept = TRUE, #automatically add in intercept if missing
  #> note: If the intercept is already contained in "random_effects", specifying
  #> intercept = FALSE does not do anything.
  # data = NULL #supply if I want to check for the existence of variables in dataframe

  #> If I haven't already created the model formula environment, do that now
  if(is.null(model_env)) {
    warning("The model environment was not pre-specified and was created.")
    model_env <- new.env(parent = globalenv())
  }
  #NOTE: the regular "build_formula()" function will also do this 1 by 1 if I
  #don't have this specified. I don't want to create a new environment for every
  #model though, so its better to pre-specify that here too.

  for (i in seq_along(model_list)) {

    if(is.null(model_list[[i]]$data)) {
      warning("Double check: ", names(model_list[i]), "; ", model_list[[i]]$name, " ",
              "No dataset was provided and no formula was created by build_formulas()")
      next #skip model
    }

    #> If I have only provided a random intercept OR a grouping variable, then
    #> flash a warning message and skip to the next model in the list
    if(!is.null(model_list[[i]]$random_effects) && is.null(model_list[[i]]$grouping_variable)) {
      warning("Double check: ", names(model_list[i]), "  ; ", model_list[[i]]$name, " ",
              "A random effects vector was provided but no grouping variable was provided.
           This model was skipped and no formula was created by build_formulas()")
      next #skip model
    }

    if(is.null(model_list[[i]]$random_effects) && !is.null(model_list[[i]]$grouping_variable)) {
      warning("Double check: ", names(model_list[i]), "; ", model_list[[i]]$name, " ",
              "A grouping variables was provided but no random effects vector was provided.
       This model was skipped and no formula was created by build_formulas()")
      next #skip model
    }




    #grab actual df from environment for call
    df <- get(model_list[[i]]$data)

    formula_i <- build_formula(
      dv = model_list[[i]]$dependent_variable,
      fixed_effects = model_list[[i]]$fixed_effects,
      random_effects = model_list[[i]]$random_effects,
      group = model_list[[i]]$grouping_variable,
      model_env = model_env, #this might cause issues here if I haven't specified it
      covary = covary,
      intercept = TRUE,
      data = df
    )

    model_list[[i]]$formula <- formula_i

  } #end of for loop

  return(model_list)
}



