
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





#' Append a Step to the Model-Comparison History
#'
#' Adds a named entry to a history list with metadata describing the action taken
#' during iterative random-effects model comparison.
#'
#' @param H A history list (typically from \code{\link{init_history}}).
#' @param step Integer step index (e.g., 0 for initial model).
#' @param type Character tag describing the step type. One of
#'   \code{"initial_model"}, \code{"drop_smallest"}, \code{"ri_vs_fixed"},
#'   \code{"add_covariances"}, \code{"final_model"}.
#' @param model1 The primary (often more complex) fitted model object for the step.
#' @param model2 The comparator fitted model object for the step (or \code{NULL}).
#' @param random_from Character vector of random-effects terms before the step.
#' @param random_to Character vector of random-effects terms after the step (or \code{NULL}).
#' @param dropped Character scalar; name of the random slope dropped on this step (if any).
#' @param mod_comp The model-comparison object (e.g., \code{anova(model1, model2)}) or \code{NULL}.
#' @param kept Character scalar indicating which model was retained at this step:
#'   \code{"m1"}, \code{"m2"}, or \code{""} for non-comparison steps.
#' @param next_action Character scalar hint for the next step:
#'   \code{"drop_smallest"}, \code{"add_covariances"}, \code{"ri_vs_fixed"}, or \code{"stop"}.
#' @param zero_pad Integer; number of digits for zero-padding the step name in the history.
#'
#' @return The updated history list \code{H} with a new named element. The name
#'   is constructed as \code{stepXX_type} and optionally includes \code{"_drop_<term>"}.
#'
#' @examples
#' H <- init_history()
#' H <- append_step_mlm_comparison(H, step = 0, type = "initial_model",
#'                  model1 = NULL, random_from = "1")
#' names(H)
#'
#' @keywords internal
#' @export
append_step_mlm_comparison <- function(H, step, type,
                                       model1, model2 = NULL,
                                       random_from, random_to = NULL,
                                       dropped = NA_character_,
                                       mod_comp = NULL,
                                       kept = "",
                                       next_action = "",
                                       zero_pad = 2 #how many zeros to add before the comparison step
) {
  H[[length(H) + 1L]] <- list(
    step        = step,          # integer step number (0 for initial)
    type        = type,          # "initial_model", "drop_smallest", "ri_vs_fixed", "add_covariances", "final_model"
    dropped     = dropped,       # name of slope removed (if any)
    random_from = random_from,   # RE terms before this step
    random_to   = random_to,     # RE terms after this step
    model1      = model1,        # "baseline" or complex side
    model2      = model2,        # comparator (if any)
    mod_comp    = mod_comp,      # anova(model1, model2) (or NULL)
    kept        = kept,          # "m1" or "m2" ("" for initial/final steps)
    next_action = next_action    # "drop_smallest", "add_covariances", "ri_vs_fixed", or "stop"
  )

  # Name the newly added element
  nm_step <- sprintf(paste0("step%0", zero_pad, "d"), step)
  nm <- if (!is.null(dropped) && !is.na(dropped) && nzchar(dropped)) {
    paste0(nm_step, "_", type, "_drop_", dropped)
  } else {
    paste0(nm_step, "_", type)
  }
  names(H)[length(H)] <- nm
  return(H)
}


#' Iterative Random-Effects Structure Comparison for Mixed Models
#'
#' Automates a pragmatic workflow for simplifying and comparing random-effects
#' structures in mixed-effects models:
#' \enumerate{
#'   \item Fit the starting model with \strong{uncorrelated} random effects.
#'   \item If there are random slopes, iteratively drop the smallest-variance slope and
#'         compare via LRT; accept the simpler model if \eqn{p > .20}.
#'   \item If a drop is not accepted (\eqn{p \le .20}), compare uncorrelated vs
#'         correlated random effects; accept the simpler uncorrelated model if
#'         \eqn{p > .20}, else the correlated model.
#'   \item If only a random intercept is present:
#'         \itemize{
#'           \item If \code{allow_drop_intercept = TRUE}, compare RI-lmer vs fixed-effects lm
#'                 using the chi-bar 1/2 adjustment (accept lm if \code{(p/2) > .20}).
#'           \item Otherwise, keep RI.
#'         }
#' }
#'
#' @param data A data frame containing all variables referenced in the model.
#' @param dv Character scalar; dependent variable name.
#' @param fixed_effects Character vector of fixed-effect terms (may include interactions).
#' @param random_effects Character vector of random-effect terms (include \code{"1"} for RI).
#' @param group Character scalar; grouping variable for random effects.
#' @param model_env Optional environment for the created formulas (default: new env with
#'   \code{globalenv()} as parent if \code{NULL}).
#' @param history Optional existing history list; if \code{NULL}, a new history is created.
#' @param counter Integer step counter for logging (default \code{1}).
#' @param allow_drop_intercept Logical; if \code{TRUE}, RI vs fixed comparison is allowed.
#'
#' @details
#' Uses \code{\link{build_formula}} to construct formulas, \code{lmerTest::lmer} to fit
#' mixed models (\code{REML = FALSE}), and \code{\link{drop_smallest_term}} (which calls
#' \code{\link{extract_smallest_slope}}) for slope-dropping steps. Model comparisons are
#' performed via \code{anova(model1, model2)}.
#'
#' @return A \emph{history} list (log book) of steps, ending with an explicit
#'   \code{type = "final_model"} entry. Use \code{\link{final_model_mlm_comparison}} to retrieve
#'   the kept fit.
#'
#' @examples
#' \dontrun{
#' H <- mlm_comparison(
#'   data = df,
#'   dv = "y",
#'   fixed_effects = c("x1", "x2"),
#'   random_effects = c("1", "x1", "x2"),
#'   group = "id",
#'   allow_drop_intercept = FALSE
#' )
#' final_fit <- final_model_mlm_comparison(H)
#' }
#'
#' @importFrom stats anova lm
#' @importFrom lme4 VarCorr
#' @importFrom lmerTest lmer
#' @seealso \code{\link{build_formula}}, \code{\link{drop_smallest_term}},
#'   \code{\link{extract_smallest_slope}}, \code{\link{final_model_mlm_comparison}},
#'   \code{\link{append_step_mlm_comparison}}, \code{\link{init_history}}
#' @export
mlm_comparison <- function(data,
                           dv,
                           fixed_effects,
                           random_effects,
                           group,
                           model_env = NULL,
                           history = NULL,
                           counter = 1,
                           allow_drop_intercept = FALSE) {

  # Initialize history if needed
  if (is.null(history)) history <- init_history()

  # Sanity: need at least an intercept
  if (is.null(random_effects) ||
      (length(random_effects) == 1 && any(random_effects == ""))) {
    stop("The random effects string is empty; at minimum, an intercept is required.")
  }

  # 0) Fit the starting model (UNCORRELATED RE)
  #    This is our "model1" baseline for subsequent comparisons.
  f1 <- build_formula(
    dv = dv,
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    group = group,
    model_env = model_env,
    covary = FALSE,     # <-- start uncorrelated
    intercept = TRUE
  )
  model1 <- lmerTest::lmer(f1, data = data, REML = FALSE)

  # Record STEP 0 (never final)
  # next_action hint: either we can try dropping slopes or (if only RI) test RI vs fixed.
  next_action_step0 <- if (length(random_effects) == 1 && identical(random_effects[1], "1")) {
    if (allow_drop_intercept) "ri_vs_fixed" else "stop"
  } else {
    "drop_smallest"
  }

  history <- append_step_mlm_comparison(
    history, step = 0, type = "initial_model",
    model1 = model1, model2 = NULL,
    random_from = random_effects, random_to = random_effects,
    mod_comp = NULL, kept = "", next_action = next_action_step0
  )


  # 1) If ONLY random intercept is present:
  if (length(random_effects) == 1 && identical(random_effects[1], "1")) {

    #check to see if we want to compare the intercept only model to one without
    #the intercept
    if (!allow_drop_intercept) {
      # We are done. Explicit "final_model" step for readability.
      history <- append_step_mlm_comparison(
        history, step = counter, type = "final_model",
        model1 = model1, model2 = NULL,
        random_from = random_effects, random_to = random_effects,
        mod_comp = NULL, kept = "m1", next_action = "stop"
      )
      return(history)
    }


    # Otherwise, compare RI lmer vs fixed-effects lm (boundary case: p/2)
    f2 <- build_formula(dv = dv,
                        fixed_effects = fixed_effects,
                        model_env = model_env)
    model2 <- lm(f2, data = data)

    mod_comp <- anova(model1, model2)

    # Chi-bar 1/2 p-value rule:
    # Keep fixed (m2) if (p/2) > .20; else keep RI (m1)
    p_g_alpha <- (mod_comp[2, ]$`Pr(>Chisq)` * .5) > .20
    kept_model <- if (p_g_alpha) "m2" else "m1"

    # Log the comparison step
    history <- append_step_mlm_comparison(
      history, step = counter, type = "ri_versus_fixed",
      model1 = model1, model2 = model2,
      random_from = random_effects, random_to = character(0),
      mod_comp = mod_comp, kept = kept_model, next_action = "stop"
    )

    # Explicit final line; model1 holds the *kept* fit for clarity
    history <- append_step_mlm_comparison(
      history, step = counter + 1, type = "final_model",
      model1 = if (kept_model == "m1") model1 else model2,
      model2 = NULL,
      random_from = if (kept_model == "m1") random_effects else character(0),
      random_to   = if (kept_model == "m1") random_effects else character(0),
      mod_comp = NULL, kept = kept_model, next_action = "stop"
    )
    return(history)
  }

  # 2) Otherwise, try dropping the smallest-variance slope (still UNCORRELATED RE)
  #    Compare full (model1) vs reduced (model2). Keep reduced if p > .20.
  updated_random_effects <- drop_smallest_term(
    lmer_fit = model1,
    group = group,
    random_effects = random_effects,
    drop_intercept = FALSE #don't drop the "1" from the output. Need to include
    #it in the original function.
  )

  f2 <- build_formula(
    dv = dv,
    fixed_effects = fixed_effects,
    random_effects = updated_random_effects,
    group = group,
    model_env = model_env,
    covary = FALSE,     # reduced but still uncorrelated
    intercept = TRUE #remember, if the random intercept is provided in the original
    #term, this call doesn't actually do anything. Setting it to FALSE won't remove
    #a random intercept if the random intercept is part of the original random
    #effect vector
  )

  model2 <- lmerTest::lmer(f2, data = data, REML = FALSE)

  mod_comp <- anova(model1, model2)

  # Accept simpler (m2) if p > .20 (i.e., removing the slope didn't hurt fit)
  p_g_alpha <- mod_comp[2, ]$`Pr(>Chisq)` > .20
  kept_model <- if (p_g_alpha) "m2" else "m1"

  # Name of slope dropped on this step (exclude intercept)
  dropped_name <- setdiff(setdiff(random_effects, "1"),
                          setdiff(updated_random_effects, "1"))
  dropped_name <- if (length(dropped_name)) dropped_name[1] else NA_character_

  # Are we now at intercept-only after the drop?
  became_intercept_only <- (length(updated_random_effects) == 1 &&
                              identical(updated_random_effects[1], "1"))

  # Terminal condition here:
  # - We kept m2 (drop accepted), AND
  # - it reduced to *just* the intercept, AND
  # - we are NOT allowed to drop the intercept.
  is_terminal_here <- isTRUE(p_g_alpha) && became_intercept_only && !allow_drop_intercept

  history <- append_step_mlm_comparison(
    history, step = counter, type = "drop_smallest",
    model1 = model1, model2 = model2,
    random_from = random_effects, random_to = updated_random_effects,
    dropped = dropped_name,
    mod_comp = mod_comp, kept = kept_model,
    next_action = if (is_terminal_here) "stop" else if (p_g_alpha) "drop_smallest" else "add_covariances"
  )

  if (is_terminal_here) {
    # Done: explicit final line (model2 is the kept, intercept-only lmer)
    history <- append_step_mlm_comparison(
      history, step = counter + 1, type = "final_model",
      model1 = model2, model2 = NULL,
      random_from = updated_random_effects, random_to = updated_random_effects,
      mod_comp = NULL, kept = "m2", next_action = "stop"
    )
    return(history)
  }

  if (p_g_alpha) {
    # Keep simpler (m2) and continue the drop process recursively
    return(
      mlm_comparison(
        data = data, dv = dv, fixed_effects = fixed_effects,
        random_effects = updated_random_effects, group = group,
        model_env = model_env,
        history = history, counter = counter + 1,
        allow_drop_intercept = allow_drop_intercept
      )
    )
  }

  # 3) If drop was NOT accepted (p <= .20),
  #    try correlating RE terms (add covariances) and compare.
  f3 <- build_formula(
    dv = dv,
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    group = group,
    model_env = model_env,
    covary = TRUE,      # <-- correlated RE
    intercept = TRUE
  )
  model3 <- lmerTest::lmer(f3, data = data, REML = FALSE)

  mod_comp2 <- anova(model1, model3)

  # Accept simpler (uncorrelated, m1) if p > .20; else accept correlated (m2)
  p_g_alpha2 <- mod_comp2[2, ]$`Pr(>Chisq)` > .20
  kept_model2 <- if (p_g_alpha2) "m1" else "m2"

  history <- append_step_mlm_comparison(
    history, step = counter, type = "add_covariances",
    model1 = model1, model2 = model3,
    random_from = random_effects, random_to = random_effects, # same RE set; just covary toggled
    mod_comp = mod_comp2, kept = kept_model2, next_action = "stop"
  )

  # Always terminal here: explicit final line
  history <- append_step_mlm_comparison(
    history, step = counter + 1, type = "final_model",
    model1 = if (kept_model2 == "m1") model1 else model3,
    model2 = NULL,
    random_from = random_effects, random_to = random_effects,
    mod_comp = NULL, kept = kept_model2, next_action = "stop"
  )

  return(history)



}



#' Extract the Final (Kept) Model from a History
#'
#' Returns the kept model from the last step in a history, preferably the entry
#' tagged as \code{type == "final_model"}. If the last step is not \code{"final_model"},
#' a warning is issued and that step's \code{model1} is returned.
#'
#' @param H A history list produced by \code{\link{mlm_comparison}} and
#'   augmented by \code{\link{append_step_mlm_comparison}}.
#'
#' @return The fitted model object designated as final (typically an \code{lmerMod}
#'   or \code{lm} object), or \code{NULL} if \code{H} is empty.
#'
#' @examples
#' # Given a populated history H:
#' # final_fit <- final_model_mlm_comparison(H)
#'
#' @keywords internal
#' @export
final_model_mlm_comparison <- function(H) {
  if (!length(H)) return(NULL)
  last <- H[[length(H)]]
  if (!identical(last$type, "final_model"))
    warning("Last step is not 'final_model'; returning its model1 anyway.")
  last$model1
}


#' Run \code{mlm_comparison()} Across a Model List
#'
#' Iterates over a list of model specifications and runs
#' \code{\link{mlm_comparison}} for each entry that defines random effects.
#' Skips entries without random effects, attaching each run's step-by-step
#' history under \code{$mc}.
#'
#' @param model_list A list of model descriptors. Each element should include:
#'   \itemize{
#'     \item \code{$name} (optional) A label used for messages.
#'     \item \code{$dependent_variable} Character; DV name.
#'     \item \code{$fixed_effects} Character vector; fixed-effect terms.
#'     \item \code{$random_effects} Character vector; random-effect terms (include \code{"1"} for RI).
#'     \item \code{$grouping_variable} Character; grouping variable for random effects.
#'     \item \code{$data} Character; name of a data frame object (resolved via \code{get()} if \code{data} arg is \code{NULL}).
#'   }
#' @param model_env Optional environment to bind formulas to. If \code{NULL},
#'   a new environment is created with \code{globalenv()} as parent (a warning is issued).
#' @param data Optional data frame overriding each entry's \code{$data} lookup.
#'   If \code{NULL}, each entry's \code{$data} is retrieved with \code{get()}.
#'
#' @details
#' For each entry with random effects, the function calls \code{\link{mlm_comparison}}
#' with \code{allow_drop_intercept = FALSE} and stores the returned history list
#' in \code{model_list[[i]]$mc}. Entries without random effects are skipped with a message.
#'
#' @return The input \code{model_list} with \code{$mc} (history log) added to
#'   each processed element.
#'
#' @examples
#' \dontrun{
#' out <- run_model_comparisons(model_list = ml)
#' # Inspect the comparison history of the first model:
#' out[[1]]$mc
#' }
#'
#' @seealso \code{\link{mlm_comparison}}
#' @export
run_model_comparisons <- function(model_list, model_env = NULL, data = NULL) {

  #> If I haven't already created the model formula environment, do that now
  if(is.null(model_env)) {
    warning("The model environment was not pre-specified and was created.")
    model_env <- new.env(parent = globalenv())
  }


  #> Per below, update mlm_comparison function and write function similar to
  #> run_models around this.
  for(i in seq_along(model_list)) {


    if(is.null(model_list[[i]]$random_effects)) {
      message(names(model_list[i]), ";", model_list[[i]]$name, ":
            did not have any random effects specified and was skipped")
      next} #skip model if the formula is null


    #> So I can supply a full model list (including lm() models), I will just
    #> look at all of the model_list elements and then


    #if I don't supply the data on the upper level, then grab the relevant data
    #frame as specified within the model_list object
    if (is.null(data)) {data <- get(model_list[[i]]$data)}

    #> grab current model:
    fixed_effects <- model_list[[i]]$fixed_effects
    random_effects <- model_list[[i]]$random_effects
    gv <- model_list[[i]]$grouping_variable
    dv <- model_list[[i]]$dependent_variable

    mc <- mlm_comparison(
      data = data,
      dv = dv,
      fixed_effects = fixed_effects,
      random_effects = random_effects,
      group = gv,
      model_env = model_env,
      history = NULL, #mlm_comparison will automatically create history log
      counter = 1,
      allow_drop_intercept = FALSE
      #don't compare random intercept to no random intercept
    )

    #add the model comparison history to the model_list object
    model_list[[i]]$mc <- mc


  }
  return(model_list)
}




#' Build an \code{mlm_report} object
#'
#' Convert an \code{mlm_comparison()} history list into a structured report for printing.
#'
#' @param history A list of steps produced by \code{mlm_comparison()}.
#' @param title Optional report title.
#' @return An object of class \code{mlm_report}.
#' @export
make_mlm_report <- function(history, title = NULL) {
  stopifnot(is.list(history), length(history) >= 1L)
  steps <- lapply(history, function(st) {
    list(
      step        = st$step,
      type        = st$type,
      dropped     = st$dropped %||% NA_character_,
      random_from = st$random_from %||% character(0),
      random_to   = st$random_to   %||% character(0),
      kept        = st$kept        %||% "",
      mod_comp    = st$mod_comp,
      comp_df     = .anova_to_df(st$mod_comp)
    )
  })
  structure(list(
    title = title %||% "Mixed Model Random-Effects Comparison",
    steps = steps
  ), class = "mlm_report")
}


#' Print method for \code{mlm_report}
#'
#' Pretty-prints the stepwise comparison, including ANOVA tables and decisions.
#'
#' @param x An \code{mlm_report} object.
#' @param digits Number formatting digits for numeric columns.
#' @param max_terms Maximum RE terms to show before truncating.
#' @param width Output width.
#' @param alpha Significance level used for annotations (informational).
#' @param ... Ignored.
#' @exportS3Method print mlm_report
print.mlm_report <- function(x, digits = 2, max_terms = 6,
                             width = getOption("width"), alpha = 0.05, ...) {
  cat("\n", x$title, "\n", sep = "")
  cat(strrep("=", nchar(x$title)), "\n\n", sep = "")

  # helper: figure out whether final model is correlated (based on prior add_covariances)
  .final_is_correlated <- function(steps, idx) {
    if (idx <= 1) return(NA)  # unknown
    prev <- steps[[idx - 1L]]
    if (!identical(prev$type, "add_covariances")) return(NA)
    # In add_covariances, kept=="m2" means correlated RE was preferred
    if (identical(prev$kept, "m2")) TRUE else if (identical(prev$kept, "m1")) FALSE else NA
  }

  for (i in seq_along(x$steps)) {
    st <- x$steps[[i]]

    type_label <- switch(st$type,
                         "initial_model"   = "Initial model (uncorrelated RE)",
                         "drop_smallest"   = "Drop smallest slope (uncorrelated RE)",
                         "add_covariances" = "Add covariances among RE (correlated RE)",
                         "ri_versus_fixed" = "Random intercept vs fixed effects",
                         "final_model"     = "Final model",
                         st$type
    )
    cat(sprintf("Step %d: %s\n", st$step, type_label))

    # RE before/after
    show_vec <- function(v) {
      if (!length(v)) return("(none)")
      if (length(v) > max_terms) {
        paste0(paste(v[seq_len(max_terms)], collapse = ", "), ", ... (", length(v), " terms)")
      } else paste(v, collapse = ", ")
    }
    cat("  RE before: ", show_vec(st$random_from), "\n", sep = "")
    if (!identical(st$type, "initial_model"))
      cat("  RE after : ", show_vec(st$random_to), "\n", sep = "")

    # dropped slope (if any)
    if (!is.null(st$dropped) && !is.na(st$dropped) && nzchar(st$dropped))
      cat("  Dropped  : ", st$dropped, "\n", sep = "")

    # --- Explicit legend for the add_covariances comparison ---
    if (identical(st$type, "add_covariances")) {
      cat("  Comparison mapping: Model 1 = UNcorrelated RE, Model 2 = CORRELATED RE\n")
    }

    # anova / comparison table
    df_num <- st$comp_df
    if (!is.null(df_num)) {
      hdr <- attr(df_num, "heading")
      df <- df_num
      if ("P_Chisq" %in% names(df)) {
        pnum <- suppressWarnings(as.numeric(df[["P_Chisq"]]))
        df[["P_Chisq"]] <- format_p(pnum)
        df[["Sig"]]     <- sig_stars(pnum)
      }
      num_cols <- vapply(df, is.numeric, logical(1))
      df[num_cols] <- lapply(df[num_cols], function(v) formatC(v, digits = digits, format = "f"))
      if (!is.null(hdr) && length(hdr))
        cat("\n  ", paste(hdr, collapse = " | "), "\n", sep = "")
      cat("\n")
      out <- data.frame(Model = rownames(df), df, check.names = FALSE, row.names = NULL)
      print(out, row.names = FALSE, right = TRUE)

      best <- if ("AIC" %in% names(df_num)) which.min(df_num$AIC) else NA_integer_
      if (!is.na(best)) cat(sprintf("\n  Best (by AIC): %s\n", rownames(df_num)[best]))

      if (identical(st$type, "ri_versus_fixed") && "P_Chisq" %in% names(df_num)) {
        p_last <- suppressWarnings(tail(as.numeric(df_num$P_Chisq), 1))
        if (length(p_last) && is.finite(p_last)) {
          cat(sprintf("  Chi-bar(1/2) adjustment: p/2 = %s\n", format_p(p_last/2)))
        }
      }
    }

    # decision line
    kept <- st$kept %||% ""
    decision <- switch(st$type,
                       "drop_smallest"   = if (identical(kept, "m2")) "Decision: DROP accepted (keep reduced model)." else
                         if (identical(kept, "m1")) "Decision: DROP rejected (retain fuller model)." else NULL,
                       "add_covariances" = if (identical(kept, "m2")) "Decision: ADD covariances (keep correlated RE)." else
                         if (identical(kept, "m1")) "Decision: Keep UNcorrelated RE (do not add covariances)." else NULL,
                       "ri_versus_fixed" = if (identical(kept, "m2")) "Decision: Prefer FIXED-effects model (drop RI)." else
                         if (identical(kept, "m1")) "Decision: Prefer RANDOM-intercept model." else NULL,
                       "final_model"     = {
                         is_corr <- .final_is_correlated(x$steps, i)
                         if (isTRUE(is_corr)) {
                           "Decision: FINAL model uses CORRELATED random effects."
                         } else if (identical(is_corr, FALSE)) {
                           "Decision: FINAL model uses UNcorrelated random effects."
                         } else {
                           "Decision: FINAL model."
                         }
                       },
                       NULL
    )
    if (!is.null(decision)) cat("  ", decision, "\n", sep = "")

    cat(strrep("-", width), "\n", sep = "")
  }
  invisible(x)
}
