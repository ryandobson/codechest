
utils::globalVariables(c("lm", "lmer", "VarCorr", "tail"))




#' Extract the Random Slope with the Smallest Variance
#'
#' Given a fitted \code{\link[lme4]{lmer}} model and a grouping factor, this
#' function inspects the random-effects variance components and returns the name
#' of the random \emph{slope} with the smallest variance. Models must be fit with
#' \strong{uncorrelated} random effects (i.e., using \code{||}); if correlated
#' random effects are detected, the function stops with an error.
#'
#' @param lmer_fit A fitted \code{lmerMod} object from \code{\link[lme4]{lmer}}.
#' @param group Character scalar giving the name of the grouping factor (the level
#'   in \code{\link[lme4]{VarCorr}} to inspect).
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Retrieves the variance-covariance components via \code{\link[lme4]{VarCorr}}.
#'   \item Requires an uncorrelated random-effects structure (uses \code{||} in the model).
#'   \item Excludes the random intercept and compares variances of random slopes only.
#'   \item Returns the slope with the smallest variance. If multiple are tied, the first is returned.
#' }
#' Two early-return cases are handled with warnings:
#' \itemize{
#'   \item If only a random intercept is present, returns \code{"1"}.
#'   \item If exactly one random slope and no intercept are present, returns that slope's name.
#' }
#'
#' @return A named character vector of length 1. The value is the name of the smallest-variance
#'   random slope; the name on that element is the (numeric) variance value coerced to a string.
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' fit <- lmer(y ~ x1 + (1 || id) + (0 + x1 || id), data = df)
#' extract_smallest_slope(fit, group = "id")
#' }
#'
#' @importFrom lme4 VarCorr
#' @seealso \code{\link{drop_smallest_term}}
#' @export
extract_smallest_slope <- function(lmer_fit, group) {

  #> This function is designed to take an lmer object and identify the random
  #> slope term with the smallest amount of variance.
  #> It outputs the name of the random slope with the least variance.

  #> In itself, I don't really need to call this function, as this function
  #> is called by the more expansive drop_smallest_term, which outputs a
  #> vector of random effects with the smallest term removed from it

  # lmer_fit = the lmer() model fit object
  # group = the grouping variable within the lmer() object (e.g., family_id or
  #participant_id)

  random_effects <- VarCorr(lmer_fit)

  #extract_smallest_slope expects a model fit with uncorrelated random effects
  #If I want to grab the smallest one in a correlated model I will have to update
  #this function or write another function.
  if(length(attr(random_effects[[group]], "correlation")) != 1) {
    stop("extract_smallest_slope() requires an lmer fit model with uncorrelated
            random effects but a model with correlated random effects was supplied.
            The original random effects contained in the model were returned")
    # random_effects <- colnames(random_effects[[group]])
    # random_effects[random_effects == "(Intercept)"] <- "1"
    # return(random_effects)
  }



  #remove the intercept term (only want to examine random slopes)
  re <- do.call(rbind, random_effects) #turn list into vector
  re <- as.data.frame(re) #turn vector into dataframe

  ### putting in a early exit/warning if there is only a random intercept or
  ### random slope and no term can be removed without model becoming a regular
  ### linear model
  if(any(names(re) %in% "(Intercept)") && nrow(re) == 1) {
    warning("extract_smallest_slope() was supplied a model that only contains
            a random intercept term. The random effects term was returned as
            only the random intercept. Check if a random slope term was expected.")
    re <- "1"  #return the random intercept
    return(re)
  }
  ###Similarly, if there is only a single random slope in the model (and no
  ### random intercept, I can't remove that here, so return that)
  if(!any(names(re) %in% "(Intercept)") && nrow(re) == 1) {
    warning("extract_smallest_slope() was supplied a model that only contains
            a random slop term:", names(re), "The random slope was returned,
            check if an intercept or other random slopers were expected")
    re <- names(re)  #return the random intercept
    return(re)
  }

  re$value <- re$`(Intercept)` #rename values row
  re$`(Intercept)` <- NULL  #remove old row
  re$variable <- rownames(re) #bring rownames into "variable"
  rownames(re) <- NULL #remove row names

  #filter out intercept row
  re <- re[!grepl("Intercept", re$variable), ]

  #grab the smallest slope variance value
  min_slope <- re[re == min(re$value), ]
  #if both values are 0, I need a fall back to select one of them:
  #this selects the first one
  if (nrow(min_slope) > 1) min_slope <- min_slope[1, ]

  #turn it into a named value
  min_var <- min_slope$variable
  min_val <- min_slope$value
  min_slope <- setNames(min_var, min_val)

  return(min_slope)
}




#' Drop the Smallest-Variance Random Slope from a Random-Effects Vector
#'
#' Removes the name of the random slope with the smallest variance from a supplied
#' character vector of random-effects terms (optionally also dropping the intercept).
#' This is a convenience wrapper around \code{\link{extract_smallest_slope}} for
#' iterative random-effects simplification.
#'
#' @param lmer_fit A fitted \code{lmerMod} object from \code{\link[lme4]{lmer}}.
#' @param group Character scalar giving the name of the grouping factor whose random
#'   effects should be inspected.
#' @param random_effects Character vector of random-effects terms (e.g., \code{c("1","x1","x2")}).
#'   The intercept, if present, should be represented as \code{"1"}.
#' @param drop_intercept Logical; if \code{TRUE}, also remove the intercept (\code{"1"})
#'   from the returned vector.
#'
#' @details
#' The smallest-variance slope term is obtained via \code{\link{extract_smallest_slope}}
#' (which requires the model to be fit with \strong{uncorrelated} random effects, i.e., \code{||}).
#' That term is then removed from \code{random_effects}. If \code{drop_intercept = TRUE},
#' the intercept term \code{"1"} is also removed.
#'
#' @return A character vector equal to \code{random_effects} with the smallest-variance
#'   slope (and optionally the intercept) removed.
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' fit <- lmer(y ~ x1 + x2 + (1 + x1 + x2 || id), data = df)
#' re_vec <- c("1", "x1", "x2")
#' drop_smallest_term(fit, group = "id", random_effects = re_vec)
#' drop_smallest_term(fit, group = "id", random_effects = re_vec, drop_intercept = TRUE)
#' }
#'
#' @seealso \code{\link{extract_smallest_slope}}
#' @export
drop_smallest_term <- function(lmer_fit, group, random_effects, drop_intercept = FALSE) {

  #> this function is designed to create a new random_effects character vector
  #> with the random slope term with the smallest amount of variance removed.

  #> lmer_fit = the lmer model fit object
  #> group = the grouping variable used in the lmer object (e.g., family_id, pid)
  #> random_effects = a character vector of random effects (including intercept or not)
  #> drop_intercept = TRUE/FALSE -- if the intercept ("1") is included in the
  #> random_effects character vector, this will drop it from the output.
  #> I don't know if there is actually a use case for this, and I can't remember
  #> why I coded it in. I thought it might be useful in the mlm_comparison function
  #> because build_formula will automatically put a random intercept back in if
  #> desired, but I specify it as false there anyway.
  #> Doesn't hurt to leave it in, perhaps a use case for it will come up, and if
  #> it doesn't I don't have to specify that argument and can leave it as its
  #> defaul.

  smallest_term <- extract_smallest_slope(lmer_fit = lmer_fit, group = group)
  #> smallest term is a character vector of length = 1 of the name of the random
  #> slope term that is smallest.

  #removing the smallest term
  re <- random_effects[random_effects != smallest_term]


  #removing intercept from random_effects if desired.
  if(drop_intercept == TRUE) {re <- re[re != "1"]}

  #return the character vector of random effects
  return(re)
}





#' @title Append a Step to the Multilevel Model Comparison History
#'
#' @description
#' Appends a new step entry to a structured model comparison `history` object.
#' Each entry records the state of a model comparison process (e.g., random effect
#' dropped, covariance added, or final model reached) and can be used to reconstruct
#' or audit the progression of a multilevel modeling workflow.
#'
#' @details
#' This function is primarily used internally by stepwise model-building or
#' comparison pipelines. It maintains an ordered list of model comparison steps,
#' each labeled and time-stamped, with metadata for tracking which effects were
#' dropped, retained, or compared.
#'
#' The resulting `history` object can be printed, summarized, or saved as a
#' reproducible record of decisions made during iterative model refinement.
#'
#' Key features include:
#' \itemize{
#'   \item Automatic creation of unique step names (e.g., `"step03_drop_slope1"`).
#'   \item Chronological ordering of steps based on numeric `step` index.
#'   \item Metadata attributes for traceability (`last_step`, `last_action`, `timestamp`).
#'   \item Optional verbose printing for live progress tracking in the console.
#' }
#'
#' @param history A list object storing prior model comparison steps.
#'   Each element is itself a list containing details about one comparison step.
#' @param step Integer or numeric value representing the current step number.
#' @param type Character string specifying the type of step (e.g.,
#'   `"initial_model"`, `"drop_smallest"`, `"add_covariances"`,
#'   `"ri_versus_fixed"`, `"final_model"`). Used for labeling and ordering.
#' @param model1 The first model in the comparison (typically the simpler model).
#' @param model2 The second model in the comparison (optional; defaults to `NULL`).
#' @param random_from Character vector of random effects in the prior model.
#' @param random_to Character vector of random effects in the updated model.
#' @param dropped Character string naming the random effect term that was dropped.
#'   Defaults to `NA_character_` when no term was dropped.
#' @param mod_comp Optional model comparison result object (e.g., from [anova()]).
#'   Can be stored for later reporting or extraction.
#' @param kept Character string indicating which model was retained (e.g., `"m1"` or `"m2"`).
#'   Defaults to an empty string.
#' @param next_action Character string describing the next planned step in the pipeline
#'   (e.g., `"stop"`, `"drop_next"`, `"add_covariances"`). Defaults to `"stop"`.
#' @param verbose Logical. If `TRUE` (default), prints a human-readable message
#'   describing the appended step to the console.
#'
#' @return
#' An updated `history` list containing the newly appended step.
#' The object includes ordered step names, updated attributes:
#' \describe{
#'   \item{`attr(., "last_step")`}{The last step number added.}
#'   \item{`attr(., "last_action")`}{The type of the last recorded action.}
#'   \item{`attr(., "timestamp")`}{The system time when the last step was appended.}
#' }
#'
#' @examples
#' \dontrun{
#' # Initialize an empty history list
#' history <- list()
#'
#' # Append an initial model step
#' history <- append_step_mlm_comparison(
#'   history = history,
#'   step = 1,
#'   type = "initial_model",
#'   model1 = "m1",
#'   random_from = c("(1|id)", "(1|group)"),
#'   random_to = c("(1|id)", "(1|group)"),
#'   next_action = "drop_smallest"
#' )
#'
#' # Append a model comparison step
#' history <- append_step_mlm_comparison(
#'   history = history,
#'   step = 2,
#'   type = "drop_smallest",
#'   model1 = "m1",
#'   model2 = "m2",
#'   random_from = c("(1|id)", "(1|group)"),
#'   random_to = c("(1|id)"),
#'   dropped = "(1|group)",
#'   kept = "m1",
#'   next_action = "final_model"
#' )
#' }
#'
#' @seealso
#' [anova()], [lme4::lmer()], [apa_anova_comparison()]
#'
#' @export
append_step_mlm_comparison <- function(history,
                                       step,
                                       type,
                                       model1,
                                       model2 = NULL,
                                       random_from,
                                       random_to,
                                       dropped = NA_character_,
                                       mod_comp = NULL,
                                       kept = "",
                                       next_action = "stop",
                                       verbose = TRUE) {
  # --- Safety checks ---
  stopifnot(is.list(history))
  if (is.null(type)) stop("`type` must be specified for append_step_mlm_comparison().")
  if (!is.numeric(step)) step <- as.integer(step)

  # --- Clean and normalize inputs ---
  random_from <- random_from %||% character(0)
  random_to   <- random_to   %||% character(0)
  dropped     <- dropped     %||% NA_character_
  kept        <- kept        %||% ""
  next_action <- next_action %||% "stop"

  # --- Build the stored object ---
  entry <- list(
    step         = step,
    type         = type,
    dropped      = dropped,
    random_from  = random_from,
    random_to    = random_to,
    model1       = model1,
    model2       = model2,
    mod_comp     = mod_comp,
    kept         = kept,
    next_action  = next_action
  )

  # --- Name the step (standardized and lexically sortable) ---
  step_prefix <- sprintf("step%02d_", step)
  suffix <- switch(
    type,
    initial_model   = "initial_model",
    final_model     = "final_model",
    drop_smallest   = {
      if (!is.na(dropped) && nzchar(dropped))
        paste0("drop_", dropped)
      else
        "drop_smallest"
    },
    add_covariances = "add_covariances",
    ri_versus_fixed = "ri_versus_fixed",
    type
  )
  step_name <- paste0(step_prefix, suffix)

  # --- Handle duplicate names safely ---
  if (step_name %in% names(history)) {
    dup_idx <- sum(grepl(step_name, names(history))) + 1L
    step_name <- paste0(step_name, "_", dup_idx)
  }

  # --- Append and re-order ---
  history[[step_name]] <- entry
  ordered_names <- names(history)[order(vapply(history, function(x) x$step, numeric(1)))]
  history <- history[ordered_names]

  # --- Add metadata for traceability ---
  attr(history, "last_step") <- step
  attr(history, "last_action") <- type
  attr(history, "timestamp") <- Sys.time()

  # --- Refined console message ---
  if (isTRUE(verbose)) {
    msg <- NULL

    # Helper: detect correlation state for final model
    get_corr_state <- function(history) {
      # look for last add_covariances step
      addcov_steps <- Filter(function(x) identical(x$type, "add_covariances"), history)
      if (length(addcov_steps) == 0L) return("uncorrelated (no covariance test)")
      last_addcov <- tail(addcov_steps, 1)[[1]]
      if (identical(last_addcov$kept, "m2")) return("correlated")
      if (identical(last_addcov$kept, "m1")) return("uncorrelated")
      "uncertain"
    }

    if (type == "initial_model") {
      msg <- sprintf("Initial model random effects: %s | next: %s",
                     paste(random_from, collapse = ", "),
                     next_action)

    } else if (type %in% c("drop_smallest", "add_covariances", "ri_versus_fixed")) {
      comp_num <- sum(vapply(history, function(x)
        x$type %in% c("drop_smallest", "add_covariances", "ri_versus_fixed"), logical(1)))
      pretty_type <- switch(type,
                            drop_smallest = "Remove random slope",
                            add_covariances = "Add covariances among RE",
                            ri_versus_fixed = "Random intercept vs fixed effects",
                            type
      )
      dropped_str <- if (!is.na(dropped) && nzchar(dropped)) paste0(" | dropped: ", dropped) else ""
      msg <- sprintf("Comparison %d: %s%s | kept: %s | next: %s",
                     comp_num, pretty_type, dropped_str, kept, next_action)

    } else if (type == "final_model") {
      corr_state <- get_corr_state(history)
      msg <- sprintf("Final model random effect structure: %s (%s)",
                     paste(random_to, collapse = ", "),
                     corr_state)
    }

    if (!is.null(msg)) cat(msg, "\n")
  }

  return(history)
}


#' @title Stepwise Random-Effects Structure Comparison for Multilevel Models
#'
#' @description
#' Performs a structured, semi-automated model comparison procedure for
#' multilevel models (typically fit via [lmerTest::lmer()]) to determine
#' the optimal random-effects structure.
#' The function iteratively compares nested models—dropping random slopes,
#' testing covariance inclusion, or comparing random-intercept-only
#' versus fixed-effects-only models—while logging all decisions in a
#' reproducible `history` object.
#'
#' @details
#' This function is designed to automate common multilevel model comparison
#' workflows where the researcher sequentially simplifies or expands the random
#' effects structure based on likelihood ratio tests.
#'
#' It supports the following step types:
#' \itemize{
#'   \item **initial_model:** fits the full random-effects structure.
#'   \item **drop_smallest:** drops the least-variant random slope term.
#'   \item **add_covariances:** tests whether allowing random-effect
#'         covariances improves model fit.
#'   \item **ri_versus_fixed:** compares random-intercept-only vs.
#'         fixed-effects-only models.
#'   \item **final_model:** logs the final retained model.
#' }
#'
#' Each step is recorded via [append_step_mlm_comparison()], producing
#' a structured `history` list with all comparisons, p-values, retained
#' models, and metadata.
#'
#' The function is recursive: when `p > .20` and further reduction is possible,
#' it automatically calls itself with the updated random-effects specification.
#'
#' @param data A data frame containing all model variables.
#' @param dv Character string. The dependent variable name.
#' @param fixed_effects Character vector of fixed-effect predictors.
#' @param random_effects Character vector specifying random effects
#'   (e.g., `c("1", "slope1", "slope2")`).
#'   Must include `"1"` to specify a random intercept.
#' @param group Character string naming the grouping factor (e.g., `"id"`).
#' @param model_env Optional environment in which to evaluate the model
#'   formula. Useful when constructing formulas dynamically. Defaults to `NULL`.
#' @param history Optional list created by [init_history()] that stores the
#'   stepwise model comparison record. If `NULL`, a new history object is
#'   initialized automatically.
#' @param counter Integer index of the current step in the recursive process.
#'   Defaults to `1`. Generally used internally.
#' @param allow_drop_intercept Logical. If `TRUE`, allows testing whether
#'   the random intercept can be dropped (i.e., compare random-intercept-only
#'   vs fixed-effects-only model). Defaults to `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress messages describing
#'   each comparison step and decision to the console. Defaults to `FALSE`.
#'
#' @return
#' A `history` list containing all model comparison steps appended by
#' [append_step_mlm_comparison()].
#' The returned object includes metadata attributes:
#' \describe{
#'   \item{`attr(., "last_step")`}{Numeric index of the final step.}
#'   \item{`attr(., "last_action")`}{String indicating the type of last action.}
#'   \item{`attr(., "timestamp")`}{POSIXct time when the final step was logged.}
#' }
#' Each entry of `history` contains:
#' \itemize{
#'   \item `model1`, `model2`: compared model objects (typically from [lmerTest::lmer()]).
#'   \item `mod_comp`: ANOVA comparison results.
#'   \item `dropped`, `kept`: random-effect terms removed or retained.
#'   \item `next_action`: string describing next comparison step.
#' }
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' data(sleepstudy)
#'
#' # Define random and fixed effects
#' dv <- "Reaction"
#' fixed_effects <- "Days"
#' random_effects <- c("1", "Days")
#' group <- "Subject"
#'
#' # Run stepwise model comparison
#' hist_obj <- mlm_comparison(
#'   data = sleepstudy,
#'   dv = dv,
#'   fixed_effects = fixed_effects,
#'   random_effects = random_effects,
#'   group = group,
#'   verbose = TRUE
#' )
#'
#' # Review summary of retained steps
#' str(hist_obj, max.level = 1)
#' }
#'
#' @seealso
#' [append_step_mlm_comparison()], [anova()], [lmerTest::lmer()]
#'
#' @export
mlm_comparison <- function (data, dv, fixed_effects, random_effects, group, model_env = NULL,
                            history = NULL, counter = 1, allow_drop_intercept = FALSE,
                            verbose = FALSE)
{
  if (is.null(history))
    history <- init_history()
  if (is.null(random_effects) || (length(random_effects) ==
                                  1 && any(random_effects == ""))) {
    stop("The random effects string is empty; at minimum, an intercept is required.")
  }
  f1 <- build_formula(dv = dv, fixed_effects = fixed_effects,
                      random_effects = random_effects, group = group, model_env = model_env,
                      covary = FALSE, intercept = TRUE)
  model1 <- lmerTest::lmer(f1, data = data, REML = FALSE)
  next_action_step0 <- if (length(random_effects) == 1 && identical(random_effects[1],
                                                                    "1")) {
    if (allow_drop_intercept)
      "ri_vs_fixed"
    else "stop"
  }
  else {
    "drop_smallest"
  }

  # Only add the initial_model step if this is the *first* call
  # (prevents duplicate "Initial Model" entries in recursive calls)
  if (length(history) == 0L) {

    history <- append_step_mlm_comparison(
      history, step = 0,
      type = "initial_model",
      model1 = model1, model2 = NULL,
      random_from = random_effects, random_to = random_effects,
      mod_comp = NULL, kept = "",
      next_action = next_action_step0,
      verbose = verbose
    )
  }  #  END of conditional addition

  if (length(random_effects) == 1 && identical(random_effects[1],
                                               "1")) {
    if (!allow_drop_intercept) {
      history <- append_step_mlm_comparison(history, step = counter,
                                            type = "final_model", model1 = model1, model2 = NULL,
                                            random_from = random_effects, random_to = random_effects,
                                            mod_comp = NULL, kept = "m1", next_action = "stop",
                                            verbose = verbose)
      return(history)
    }
    f2 <- build_formula(dv = dv, fixed_effects = fixed_effects,
                        model_env = model_env)
    model2 <- lm(f2, data = data)
    mod_comp <- anova(model1, model2)
    p_g_alpha <- (mod_comp[2, ]$`Pr(>Chisq)` * 0.5) > 0.2
    kept_model <- if (p_g_alpha)
      "m2"
    else "m1"
    history <- append_step_mlm_comparison(history, step = counter,
                                          type = "ri_versus_fixed", model1 = model1, model2 = model2,
                                          random_from = random_effects, random_to = character(0),
                                          mod_comp = mod_comp, kept = kept_model, next_action = "stop",
                                          verbose = verbose)
    history <- append_step_mlm_comparison(history, step = counter +
                                            1, type = "final_model", model1 = if (kept_model ==
                                                                                  "m1")
                                              model1
                                          else model2, model2 = NULL, random_from = if (kept_model ==
                                                                                        "m1")
                                            random_effects
                                          else character(0), random_to = if (kept_model == "m1")
                                            random_effects
                                          else character(0), mod_comp = NULL, kept = kept_model,
                                          next_action = "stop",
                                          verbose = verbose)
    return(history)
  }
  updated_random_effects <- drop_smallest_term(lmer_fit = model1,
                                               group = group, random_effects = random_effects, drop_intercept = FALSE)
  f2 <- build_formula(dv = dv, fixed_effects = fixed_effects,
                      random_effects = updated_random_effects, group = group,
                      model_env = model_env, covary = FALSE, intercept = TRUE)
  model2 <- lmerTest::lmer(f2, data = data, REML = FALSE)
  mod_comp <- anova(model1, model2)
  p_g_alpha <- mod_comp[2, ]$`Pr(>Chisq)` > 0.2
  kept_model <- if (p_g_alpha)
    "m2"
  else "m1"
  dropped_name <- setdiff(setdiff(random_effects, "1"), setdiff(updated_random_effects,
                                                                "1"))
  dropped_name <- if (length(dropped_name))
    dropped_name[1]
  else NA_character_
  became_intercept_only <- (length(updated_random_effects) ==
                              1 && identical(updated_random_effects[1], "1"))
  is_terminal_here <- isTRUE(p_g_alpha) && became_intercept_only &&
    !allow_drop_intercept
  history <- append_step_mlm_comparison(history, step = counter,
                                        type = "drop_smallest", model1 = model1, model2 = model2,
                                        random_from = random_effects, random_to = updated_random_effects,
                                        dropped = dropped_name, mod_comp = mod_comp, kept = kept_model,
                                        next_action = if (is_terminal_here)
                                          "stop"
                                        else if (p_g_alpha)
                                          "drop_smallest"
                                        else "add_covariances",
                                        verbose = verbose)
  if (is_terminal_here) {
    history <- append_step_mlm_comparison(history, step = counter +
                                            1, type = "final_model", model1 = model2, model2 = NULL,
                                          random_from = updated_random_effects, random_to = updated_random_effects,
                                          mod_comp = NULL, kept = "m2", next_action = "stop",
                                          verbose = verbose)
    return(history)
  }
  if (p_g_alpha) {
    return(mlm_comparison(data = data, dv = dv, fixed_effects = fixed_effects,
                          random_effects = updated_random_effects, group = group,
                          model_env = model_env, history = history, counter = counter +
                            1, allow_drop_intercept = allow_drop_intercept,
                          verbose = verbose))
  }
  f3 <- build_formula(dv = dv, fixed_effects = fixed_effects,
                      random_effects = random_effects, group = group, model_env = model_env,
                      covary = TRUE, intercept = TRUE)
  model3 <- lmerTest::lmer(f3, data = data, REML = FALSE)
  mod_comp2 <- anova(model1, model3)
  p_g_alpha2 <- mod_comp2[2, ]$`Pr(>Chisq)` > 0.2
  kept_model2 <- if (p_g_alpha2)
    "m1"
  else "m2"
  history <- append_step_mlm_comparison(history, step = counter,
                                        type = "add_covariances", model1 = model1, model2 = model3,
                                        random_from = random_effects, random_to = random_effects,
                                        mod_comp = mod_comp2, kept = kept_model2, next_action = "stop",
                                        verbose = verbose)
  history <- append_step_mlm_comparison(history, step = counter +
                                          1, type = "final_model", model1 = if (kept_model2 ==
                                                                                "m1")
                                            model1
                                        else model3, model2 = NULL, random_from = random_effects,
                                        random_to = random_effects, mod_comp = NULL, kept = kept_model2,
                                        next_action = "stop",
                                        verbose = verbose)
  return(history)
}



#' @title Extract the Final Fitted Model from an MLM Comparison History
#'
#' @description
#' Retrieves the final retained model object (usually a fitted
#' [lmerTest::lmer()] model) from a multilevel model comparison history
#' created by [mlm_comparison()].
#'
#' @details
#' This function is a lightweight helper designed to recover the final model
#' selected in a stepwise model comparison workflow.
#' It locates the most recent step of type `"final_model"` within the stored
#' `mlm_history` object. If no `"final_model"` step exists, it returns the
#' `model1` component of the last recorded step, with a warning.
#'
#' @param mlm_history A list object representing the model comparison history
#'   (typically returned by [mlm_comparison()]). Must contain one or more step
#'   entries created via [append_step_mlm_comparison()].
#'
#' @return
#' The final fitted model object stored in the last `"final_model"` step.
#' If no `"final_model"` step is found, returns the `model1` from the last
#' available step.
#' Returns `NULL` (with a warning) if the history object is empty.
#'
#' @examples
#' \dontrun{
#' # Example workflow
#' history <- mlm_comparison(
#'   data = sleepstudy,
#'   dv = "Reaction",
#'   fixed_effects = "Days",
#'   random_effects = c("1", "Days"),
#'   group = "Subject"
#' )
#'
#' # Extract final retained model
#' final_mod <- final_model_mlm_comparison(history)
#' summary(final_mod)
#' }
#'
#' @seealso
#' [mlm_comparison()], [append_step_mlm_comparison()]
#'
#' @export

final_model_mlm_comparison <- function(mlm_history) {

  H <- mlm_history
  if (!length(H)) {
    warning("History object is empty; returning NULL.")
    return(NULL)
  }

  # Find the last step
  last <- H[[length(H)]]

  # If the last step isn't the final_model, try to locate one
  if (!identical(last$type, "final_model")) {
    final_idx <- which(vapply(H, function(x) identical(x$type, "final_model"), logical(1)))
    if (length(final_idx)) {
      last <- H[[tail(final_idx, 1)]]
    } else {
      warning("No 'final_model' step found; returning last available model1.")
    }
  }

  # Return the stored model1 (final fitted model)
  return(last$model1)
}

#' @title Run Stepwise MLM Comparisons Across a List of Model Specifications
#'
#' @description
#' Iterates over a list of model specifications and performs a
#' stepwise random-effects comparison for each using [mlm_comparison()].
#' This function automates batch evaluation of multiple dependent variables
#' or modeling setups, appending a model comparison history (`$mc`) to each
#' entry in the provided list.
#'
#' @details
#' Each element of `model_list` should be a named list containing, at minimum:
#' \itemize{
#'   \item `name`: a descriptive label for the model.
#'   \item `data`: the data frame name or object containing the model variables.
#'   \item `fixed_effects`: a character vector of fixed-effect predictors.
#'   \item `random_effects`: a character vector of random-effect terms (e.g., `c("1", "slope1")`).
#'   \item `grouping_variable`: the grouping factor for the multilevel model.
#'   \item `dependent_variable`: the dependent variable name.
#' }
#'
#' The function automatically creates a new environment if none is provided via
#' `model_env`. Each model’s comparison is run using [mlm_comparison()],
#' and the resulting comparison history is stored under the `$mc` element.
#'
#' Models without specified random effects are skipped with a message.
#'
#' @param model_list A named list of model specifications.
#'   Each element must include the components described above.
#' @param model_env An optional environment in which model formulas are built
#'   and evaluated. If `NULL`, a new environment is created with
#'   `parent = globalenv()`.
#' @param data Optional data frame. If `NULL`, each model entry’s
#'   `$data` element is retrieved by name using [base::get()].
#' @param verbose Logical. If `TRUE`, prints detailed messages from
#'   [mlm_comparison()] as it runs. Defaults to `FALSE`.
#'
#' @return
#' A list identical in structure to the input `model_list`, but with an
#' additional component `$mc` appended to each model specification.
#' The `$mc` element stores the model comparison history returned by
#' [mlm_comparison()].
#'
#' @examples
#' \dontrun{
#' # Define two example model specifications
#' model_list <- list(
#'   ModelA = list(
#'     name = "Reaction ~ Days",
#'     data = "sleepstudy",
#'     dependent_variable = "Reaction",
#'     fixed_effects = "Days",
#'     random_effects = c("1", "Days"),
#'     grouping_variable = "Subject"
#'   ),
#'   ModelB = list(
#'     name = "Reaction ~ 1",
#'     data = "sleepstudy",
#'     dependent_variable = "Reaction",
#'     fixed_effects = "1",
#'     random_effects = "1",
#'     grouping_variable = "Subject"
#'   )
#' )
#'
#' # Run comparisons for both
#' results <- run_mlm_comparisons(model_list, verbose = TRUE)
#'
#' # Access the comparison history for the first model
#' results$ModelA$mc
#' }
#'
#' @seealso
#' [mlm_comparison()], [append_step_mlm_comparison()]
#'
#' @export
run_mlm_comparisons <- function (model_list, model_env = NULL, data = NULL, verbose = FALSE)
{
  if (is.null(model_env)) {
    warning("The model environment was not pre-specified and was created.")
    model_env <- new.env(parent = globalenv())
  }
  for (i in seq_along(model_list)) {
    if (is.null(model_list[[i]]$random_effects)) {
      message(names(model_list[i]), ";", model_list[[i]]$name,
              ":\n            did not have any random effects specified and was skipped")
      next
    }
    if (is.null(data)) {
      data <- get(model_list[[i]]$data)
    }
    fixed_effects <- model_list[[i]]$fixed_effects
    random_effects <- model_list[[i]]$random_effects
    gv <- model_list[[i]]$grouping_variable
    dv <- model_list[[i]]$dependent_variable
    mc <- mlm_comparison(data = data, dv = dv, fixed_effects = fixed_effects,
                         random_effects = random_effects, group = gv, model_env = model_env,
                         history = NULL, counter = 1, allow_drop_intercept = FALSE,
                         verbose = verbose)
    model_list[[i]]$mc <- mc
  }
  return(model_list)
}




#' @title Create a Structured Report Object from an MLM Comparison History
#'
#' @description
#' Converts a full model comparison `history` (produced by [mlm_comparison()])
#' into a standardized report object of class `"mlm_report"`.
#' The resulting object stores key information about each comparison step
#' (e.g., dropped effects, retained models, test statistics) and can be
#' rendered or summarized in APA-style tables using downstream functions.
#'
#' @details
#' The report provides a compact representation of all model-building steps
#' for reproducible documentation and reporting.
#' It extracts core elements from each step in the `history` object—such as
#' model types, random-effects transitions, dropped terms, and comparison
#' outcomes—and stores them in a consistent structure suitable for export,
#' tabulation, or diagnostic display.
#'
#' The internal helper `.anova_to_df()` is used to safely convert likelihood
#' ratio test results into data-frame form for inclusion in the report.
#'
#' @param history A list object created by [mlm_comparison()] and updated
#'   through [append_step_mlm_comparison()]. Must contain at least one
#'   recorded step.
#' @param title Optional character string giving the report title.
#'   Defaults to `"Mixed Model Random-Effects Comparison"`.
#' @param verbosity Character string indicating the level of report detail.
#'   Must be one of `"short"` or `"long"`. Defaults to `"short"`.
#'
#' @return
#' An object of class `"mlm_report"`, containing:
#' \describe{
#'   \item{`title`}{Report title used in output.}
#'   \item{`steps`}{A list of extracted step summaries (one per comparison).}
#'   \item{`verbosity`}{Character string indicating the desired reporting depth.}
#' }
#' The `"mlm_report"` object can be passed to higher-level rendering or
#' formatting functions (e.g., `print.mlm_report()` or `save_apa_mlm_report()`).
#'
#' @examples
#' \dontrun{
#' # Fit and compare models
#' hist_obj <- mlm_comparison(
#'   data = sleepstudy,
#'   dv = "Reaction",
#'   fixed_effects = "Days",
#'   random_effects = c("1", "Days"),
#'   group = "Subject"
#' )
#'
#' # Create structured report
#' report <- make_mlm_report(hist_obj, title = "Random-Effects Model Comparison")
#'
#' # Inspect structure
#' str(report, max.level = 1)
#' }
#'
#' @seealso
#' [mlm_comparison()], [append_step_mlm_comparison()], [run_mlm_comparisons()]
#'
#' @export

make_mlm_report <- function(history,
                            title = NULL,
                            verbosity = c("short", "long")) {
  verbosity <- match.arg(verbosity)
  stopifnot(is.list(history), length(history) >= 1L)

  steps <- lapply(history, function(st) {
    list(
      step = st$step,
      type = st$type,
      dropped = st$dropped %||% NA_character_,
      random_from = st$random_from %||% character(0),
      random_to = st$random_to %||% character(0),
      kept = st$kept %||% "",
      mod_comp = st$mod_comp,
      comp_df = .anova_to_df(st$mod_comp)
    )
  })

  structure(
    list(
      title = title %||% "Mixed Model Random-Effects Comparison",
      steps = steps,
      verbosity = verbosity
    ),
    class = "mlm_report"
  )
}


#' @title Print Method for `mlm_report` Objects
#'
#' @description
#' Nicely formats and prints an `"mlm_report"` object created by [make_mlm_report()],
#' displaying a step-by-step summary of the multilevel model comparison process.
#' The output can be shown in either `"short"` or `"long"` form, depending on
#' the verbosity level stored in the report.
#'
#' @details
#' This print method is primarily for console output and diagnostic review.
#' In `"short"` mode, it provides a compact overview of key model comparison steps
#' (initial model, comparison count, decisions, and final model).
#' In `"long"` mode, it displays detailed per-step output, including likelihood ratio
#' test tables, random-effects structures, and decision rationale based on model
#' comparisons.
#'
#' The method automatically detects covariance comparisons (`add_covariances`)
#' and annotates the final model as correlated or uncorrelated when possible.
#'
#' @param x An object of class `"mlm_report"`, typically created by
#'   [make_mlm_report()].
#' @param digits Integer specifying the number of decimal places for
#'   numeric output. Defaults to `2`.
#' @param max_terms Integer specifying how many random-effect terms to
#'   display before truncating with an ellipsis. Defaults to `6`.
#' @param width Numeric. Output width passed to `cat()` for formatting
#'   separators. Defaults to `getOption("width")`.
#' @param alpha Numeric significance threshold used for annotating results
#'   (currently used internally when displaying p-values). Defaults to `0.05`.
#' @param ... Additional arguments (ignored, for method compatibility).
#'
#' @return
#' Invisibly returns the input `"mlm_report"` object after printing its formatted
#' summary to the console.
#'
#' @examples
#' \dontrun{
#' # Fit models and generate comparison report
#' hist_obj <- mlm_comparison(
#'   data = sleepstudy,
#'   dv = "Reaction",
#'   fixed_effects = "Days",
#'   random_effects = c("1", "Days"),
#'   group = "Subject"
#' )
#' report <- make_mlm_report(hist_obj, title = "Random-Effects Model Comparison")
#'
#' # Print in long form
#' print(report, digits = 3)
#'
#' # Print in short summary mode
#' report$verbosity <- "short"
#' print(report)
#' }
#'
#' @seealso
#' [make_mlm_report()], [mlm_comparison()], [append_step_mlm_comparison()]
#'
#' @export
print.mlm_report <- function(x,
                             digits = 2,
                             max_terms = 6,
                             width = getOption("width"),
                             alpha = 0.05,
                             ...) {

  cat("\n", x$title, "\n", sep = "")
  cat(strrep("=", nchar(x$title)), "\n\n", sep = "")
  verbosity <- x$verbosity %||% "long"

  .show_vec <- function(v, max_terms = 6) {
    if (!length(v)) return("(none)")
    if (length(v) > max_terms)
      paste0(paste(v[seq_len(max_terms)], collapse = ", "),
             ", ... (", length(v), " terms)")
    else paste(v, collapse = ", ")
  }

  .final_is_correlated <- function(steps, idx) {
    if (idx <= 1) return(NA)
    prev <- steps[[idx - 1L]]
    if (!identical(prev$type, "add_covariances")) return(NA)
    if (identical(prev$kept, "m2")) TRUE
    else if (identical(prev$kept, "m1")) FALSE
    else NA
  }

  # ---- iterate through steps ----
  for (i in seq_along(x$steps)) {
    st <- x$steps[[i]]

    ## ---------------- SHORT MODE ---------------- ##
    if (verbosity == "short") {
      if (identical(st$type, "initial_model")) {
        cat(sprintf("Initial model: %s (uncorrelated)\n",
                    .show_vec(st$random_from)))
      } else if (st$type %in% c("drop_smallest", "add_covariances", "ri_versus_fixed")) {
        comp_num <- sum(vapply(x$steps[seq_len(i)],
                               function(s) s$type %in% c("drop_smallest","add_covariances","ri_versus_fixed"),
                               logical(1)))
        cat(sprintf("Comparison %d: %s | kept: %s\n",
                    comp_num,
                    switch(st$type,
                           drop_smallest="Remove random slope",
                           add_covariances="Add covariances",
                           ri_versus_fixed="Random intercept vs fixed effects",
                           st$type),
                    st$kept))
      } else if (identical(st$type, "final_model")) {
        is_corr <- .final_is_correlated(x$steps, i)
        corr_txt <- if (isTRUE(is_corr)) "(correlated)"
        else if (identical(is_corr, FALSE)) "(uncorrelated)"
        else ""
        cat(sprintf("Final model: %s %s\n",
                    .show_vec(st$random_to), corr_txt))
      }
      next
    }

    ## ---------------- LONG MODE ---------------- ##
    if (identical(st$type, "initial_model")) {
      cat("Initial Model (uncorrelated random effects)\n")
      cat("  Random effect structure: ", .show_vec(st$random_from), "\n", sep = "")
      cat(strrep("-", width), "\n", sep = "")
      next
    }

    if (st$type %in% c("drop_smallest", "add_covariances", "ri_versus_fixed")) {
      comp_num <- sum(vapply(x$steps[seq_len(i)],
                             function(s) s$type %in% c("drop_smallest","add_covariances","ri_versus_fixed"),
                             logical(1)))
      cat(sprintf("Comparison %d: %s\n", comp_num,
                  switch(st$type,
                         drop_smallest   = "Remove random slope",
                         add_covariances = "Add covariances among random effects",
                         ri_versus_fixed = "Random intercept vs fixed effects",
                         st$type)))

      corr1 <- if (identical(st$type, "add_covariances")) "(uncorrelated)" else ""
      corr2 <- if (identical(st$type, "add_covariances")) "(correlated)" else ""
      cat("  Model 1 structure: ", .show_vec(st$random_from), " ", corr1, "\n", sep = "")
      cat("  Model 2 structure: ", .show_vec(st$random_to),   " ", corr2, "\n", sep = "")

      if (!is.null(st$comp_df)) {
        df_num <- st$comp_df
        hdr <- attr(df_num, "heading")
        if (!is.null(hdr) && length(hdr))
          cat("\n  ", paste(hdr, collapse = " | "), "\n", sep = "")
        df <- df_num
        if ("P_Chisq" %in% names(df)) {
          pnum <- suppressWarnings(as.numeric(df[["P_Chisq"]]))
          df[["P_Chisq"]] <- format_p(pnum)
          df[["Sig"]] <- sig_stars(pnum)
        }
        num_cols <- vapply(df, is.numeric, logical(1))
        df[num_cols] <- lapply(df[num_cols],
                               function(v) formatC(v, digits = digits, format = "f"))
        cat("\n")
        out <- data.frame(Model = rownames(df), df,
                          check.names = FALSE, row.names = NULL)
        print(out, row.names = FALSE, right = TRUE)
      }

      kept <- st$kept %||% ""
      decision <- switch(st$type,
                         drop_smallest =
                           if (identical(kept, "m2")) "Decision: DROP accepted (retain reduced model)."
                         else if (identical(kept, "m1")) "Decision: DROP rejected (retain fuller model).",
                         add_covariances =
                           if (identical(kept, "m2")) "Decision: ADD covariances (keep correlated RE)."
                         else if (identical(kept, "m1")) "Decision: Do NOT add covariances (keep uncorrelated RE).",
                         ri_versus_fixed =
                           if (identical(kept, "m2")) "Decision: Prefer FIXED-effects model (drop random intercept)."
                         else if (identical(kept, "m1")) "Decision: Prefer RANDOM-intercept model.",
                         NULL
      )
      if (!is.null(decision))
        cat("\n  ", decision, "\n", sep = "")
      cat(strrep("-", width), "\n", sep = "")
      next
    }

    if (identical(st$type, "final_model")) {
      is_corr <- .final_is_correlated(x$steps, i)
      corr_txt <- if (isTRUE(is_corr)) "(correlated)"
      else if (identical(is_corr, FALSE)) "(uncorrelated)"
      else ""
      cat("Final Model\n")
      cat("  Random effect structure: ", .show_vec(st$random_to), " ", corr_txt, "\n", sep = "")
      cat(strrep("-", width), "\n", sep = "")
    }
  }

  invisible(x)
}

