
utils::globalVariables(c("capture.output"))



#' Tag a history object as \code{fixed_effect_drop_history}
#'
#' Adds the class \code{"fixed_effect_drop_history"} to an existing object (typically
#' a list of logged steps) so methods and printers can recognize it.
#'
#' @param x An object to tag (usually a list produced while dropping fixed-effect interactions).
#'
#' @return \code{x} with class \code{"fixed_effect_drop_history"} prepended.
#'
#' @examples
#' \dontrun{
#' H <- list()
#' H <- fed_set_class(H)
#' class(H)
#' }
#'
#' @keywords internal
fed_set_class <- function(x) {
  class(x) <- unique(c("fixed_effect_drop_history", class(x)))
  x
}



#' Log a step in the fixed-effect interaction dropping workflow
#'
#' Appends a named entry to a history list capturing the current step, the model,
#' the fixed effects considered, any terms queued for removal, and an optional
#' model-comparison object.
#'
#' @param history A list used as the running history/log.
#' @param step Integer step number (e.g., 0 for the initial state).
#' @param type Character step label, e.g. \code{"initial_model"}, \code{"edited_model"},
#'   \code{"final_model"}.
#' @param model The fitted model object at this step (e.g., \code{lmerMod}, \code{lm}).
#' @param fixed_effects Character vector of fixed-effect terms represented at this step.
#' @param to_get_removed Character vector of interaction terms queued to remove next,
#'   or \code{NULL} if none.
#' @param mod_comp Optional model-comparison result (e.g., from \code{anova()}), or \code{NULL}.
#' @param name_prefix Optional character prefix for the history entry name; if \code{NULL},
#'   names are generated like \code{"s00_initial_model"}.
#'
#' @return The updated \code{history} list with a new named entry.
#'
#' @examples
#' \dontrun{
#' H <- list()
#' H <- log_fixed_effect_drop(H, step = 0, type = "initial_model",
#'                      model = NULL, fixed_effects = c("x1","x2","x1*x2"),
#'                      to_get_removed = "x1*x2", mod_comp = NULL)
#' names(H)
#'  }
#' @seealso \code{\link{fixed_effect_drops}}
#' @keywords internal
log_fixed_effect_drop <- function(history,
                            step,
                            type,
                            model,
                            fixed_effects,
                            to_get_removed,
                            mod_comp,
                            name_prefix = NULL) {
  entry <- list(
    step           = step,
    type           = type,
    model          = model,          # lmerMod object kept as-is
    fixed_effects  = fixed_effects,  # character vector
    to_get_removed = to_get_removed, # character vector or NULL
    mod_comp       = mod_comp        # anova table or NULL
  )
  nm <- if (is.null(name_prefix)) {
    sprintf("s%02d_%s", step, type)              # e.g., "s00_initial_model"
  } else {
    sprintf("%s__s%02d_%s", name_prefix, step, type)  # e.g., "EP_PRCPSMSy__s03_edited_model"
  }

  history[[nm]] <- entry
  return(history)
}



#' Iteratively drop non-significant fixed effects and refit
#'
#' Starting from a fitted mixed model, this function:
#' (1) identifies non-significant interaction terms (by p-value > \code{alpha}),
#' (2) iteratively removes them while refitting the model (REML),
#' (3) optionally drops a target main effect if no significant interactions remain
#' and the main effect itself is non-significant, and
#' (4) returns a step-by-step history with a final model refit that preserves
#' the original random-effects structure.
#'
#' @param model A fitted mixed-effects model (typically \code{lmerMod}) providing
#'   the starting point and random-effects structure.
#' @param fixed_effects Character vector of fixed-effect terms to use in refits.
#' @param remove Character scalar: the variable name whose interaction or main
#'   terms should be considered for removal (passed to the internal extractor),
#'   or \code{"all_interactions"} to consider all interactions.
#' @param new_random_effect Character scalar; a random-slope variable to add to the
#'   refit during the iterative stage (added consistently with the model's current
#'   random-effects structure).
#' @param alpha Numeric threshold; interactions with p-values \emph{greater than}
#'   this are considered non-significant and eligible to drop.
#' @param model_env Optional environment to bind generated formulas; if \code{NULL},
#'   a new environment is created with \code{globalenv()} as parent (a warning is issued).
#' @param data Optional data frame used when refitting models and checking variables.
#'
#' @details
#' Internally, the function:
#' \itemize{
#'   \item Uses \code{.grab_dv()} and \code{.grab_random_int_group()} to recover the DV and grouping factor.
#'   \item Calls \code{add_random_term()} to produce a random-effects term that includes
#'         \code{new_random_effect} in a way that matches the current correlation/intercept status.
#'   \item Uses \code{drop_nonsig_int_effects()} to choose interaction terms to remove, then
#'         \code{.protect2way()} to avoid dropping two-way interactions that are part of any
#'         three-way interaction.
#'   \item Rebuilds a formula each iteration and refits with \code{lmerTest::lmer(..., REML = TRUE)}.
#'   \item After the loop, may drop \code{remove} main effect if it and all of its interactions are non-significant.
#'   \item Builds a final model via \code{build_formula()} using the \emph{original}
#'         random-effects terms and their original correlation/intercept behavior.
#' }
#'
#' @return A list representing the step-by-step history (class \code{"fixed_effect_drop_history"}),
#'   including an explicit \code{type = "final_model"} entry containing the final fitted model.
#'
#' @examples
#' \dontrun{
#' fed_hist <- fixed_effect_drops(
#'   model            = fit,
#'   fixed_effects    = c("x1","x2","x1*x2"),
#'   remove           = "x1",
#'   new_random_effect= "x1",
#'   alpha            = 0.10,
#'   data             = df
#' )
#' print(fed_hist)
#' }
#'
#' @seealso \code{\link{log_fixed_effect_drop}}, \code{\link{fed_set_class}},
#'   \code{\link{drop_nonsig_int_effects}}, \code{\link{build_fixed_term}},
#'   \code{\link{build_formula}}, \code{\link{check_random_effects}},
#'   \code{\link{get_random_effects}}, \code{\link{add_random_term}}
#' @importFrom stats reformulate
#' @importFrom lmerTest lmer
#' @export
fixed_effect_drops <- function(model,
                                   fixed_effects,
                                   remove,
                                   new_random_effect,
                                   alpha,
                                   model_env = NULL,
                                   data = NULL) {

  if(is.null(model_env)) {
    warning("The model environment was not pre-specified and was created.")
    model_env <- new.env(parent = globalenv())
  }

  #initialize history
  history <- init_history()

  #grab information from model
  dv <- grab_dv(model) #dependent variable
  group <- grab_random_int_group(model) #random intercept group variable name

  #add_random_term calls two other main functions I have written:
  #> (1) check_random_effects()
  #> (2) get_random_effects() -- this function uses output object of
  #> check_random_effects()
  random_part <- add_random_term(new_random_effect = new_random_effect,
                                 model = model,
                                 group = group,
                                 data = data)
  #> this (above) takes the original model and adds a new random effect to it.

  drop1 <- drop_nonsig_int_effects(model,
                                   fixed_effects = fixed_effects,
                                   alpha = alpha,
                                   var = remove)
  fixed_effects_to_drop <- .protect2way(drop1) #proper removal terms
  new_fixed_effects <- drop1$new_model_terms


  #first update to the history log
  counter <- 0
  history <- log_fixed_effect_drop(history,
                             step            = counter,
                             type            = "initial_model",
                             model           = model,
                             fixed_effects   = fixed_effects,
                             to_get_removed  = fixed_effects_to_drop,
                             mod_comp        = NULL,
                             name_prefix = NULL #see log for automatic name
  )

  #specifying it as null here helps navigate the if statement after the loop.
  #if the while loop never runs (becuase there are no fixed_effects_to_drop),
  #then I use the "model" supplied to the function, rather than the most recent
  #"refit" model.
  refit <- NULL  # before the while loop

  while(!is.null(fixed_effects_to_drop)) {

    #> putting in a safety stop to cut out the loop in case something goes awry
    #> 8 different models is probably a high number anyway
    counter <- counter + 1
    if(counter > 8) {
      warning("More than 8 model iteration removal events happened and the loop
             was broken")
      break
    }

    fixed_part <- build_fixed_term(fixed_effects = new_fixed_effects, data = data)

    #formula to use for analyses
    f <- reformulate(c(fixed_part, random_part), response = dv, env = model_env)

    refit <- lmerTest::lmer(f, data = data, REML = TRUE)

    drop_while <- drop_nonsig_int_effects(
      refit,
      fixed_effects = new_fixed_effects,
      alpha         = alpha,
      var           = remove)

    #> These are updated for the next iteration
    fixed_effects_to_drop <- .protect2way(drop_while) #proper removal terms


    #> I tried to add in a statement that also did model comparisons, but I was
    #> having issues with an error of "all models must be fit to the same data object."
    #> ChatGPT told me it likely arises from the models changing slightly because
    #> of missing data. Just going to not do model comparisons for the time being.
    #> if its the first comparison loop, compare the original model to the refit
    # if (counter == 1) {mod_comp <- anova(model, refit)}
    # #> If its past the first loop, compare the previous fit to the current refit
    # if (counter > 1) {mod_comp <- anova(previous_fit, refit)}
    mod_comp <- NULL


    history <- log_fixed_effect_drop(history,
                               step            = counter,
                               type            = "edited_model",
                               model           = refit,
                               fixed_effects   = new_fixed_effects,
                               to_get_removed  = fixed_effects_to_drop,
                               mod_comp        = mod_comp,
                               name_prefix     = NULL #see log for automatic name
    )

    #> Updating this after the log happens
    new_fixed_effects <- drop_while$new_model_terms


    #> Becaue I'm not doing model comparisons, I don't need to save this duplicate
    #save a duplicate of refit so the next iteration doesn't overwrite it and I
    #can compare refit to the previous fit
    #previous_fit <- refit

  }

  #> A final step is to remove the menses main effect, but only if there are no
  #> significant menses interaction terms:

  #If the while loop never runs, refit is assigned null, and then I want to use
  #the original model.
  fit_for_check <- if (is.null(refit)) model else refit
  #return(fit_for_check)
  any_interactions <- grab_int_coefs(fit_for_check, alpha = 0.1, var = remove)

  #If there are any significiant interaction terms (in this case, with the
  #variable to be removed, then continue on. If there are NO interaction terms,
  #indicated by an empty character vector of length 0, then remove menses term)
  if (length(any_interactions) == 0 &&
      #if there are no interactions, then check to see if the main effect is
      #significant.
      !check_significance(fit_for_check, remove, sig_value = 0.1)) {

       new_fixed_effects <- drop_main_effect(new_fixed_effects,
                                          remove)
  }

  #> I now need to build a new model formula that has
  #> (1) original random effects provided
  #> (2) updated fixed effects
  #> (3) is fit with REML
  orig_model_info <- check_random_effects(model) #this is the first model
  #provided to the function to search for random effects
  random_effects <- get_random_effects(orig_model_info) #this function grabs the
  #random effects from the output from "check_random_effects"
  act <- .random_structure_action(orig_model_info) #this tells what action to
  #proceed with based upon the random effect structure present. This object
  #is then fed into the build formula.

  final_f <- build_formula(dv = dv,
                           fixed_effects = new_fixed_effects,
                           random_effects = random_effects,
                           group = group,
                           model_env = model_env,
                           covary = act$covary,
                           intercept = act$intercept,
                           data = data)

  final_model <- lmerTest::lmer(final_f, data = data)

  history <- log_fixed_effect_drop(history,
                             step            = counter + 1,
                             type            = "final_model",
                             model           = final_model,
                             fixed_effects   = new_fixed_effects,
                             to_get_removed  = NULL,
                             mod_comp        = NULL,
                             name_prefix     = NULL #see log for automatic name
  )

  #set the special class for printing purposes
  history <- fed_set_class(history)
  return(history)
}




#' Run fixed-effect interaction drops across a model list
#'
#' Iterates over \code{model_list}, extracts the starting model from each entry's
#' mixed-model comparison history via \code{\link{final_model_mlm_comparison}},
#' and applies \code{\link{fixed_effect_drops}} to remove non-significant
#' interaction terms from the fixed-effects set. The resulting stepwise history
#' is stored back on each entry.
#'
#' @param model_list A list where each element represents a model specification
#'   and results. Each element is expected to contain at least:
#'   \itemize{
#'     \item \code{$mc}: a history produced by \code{mlm_comparison()} (used by
#'           \code{final_model_mlm_comparison()} to retrieve the starting model).
#'     \item a fixed-effects vector available at \code{fes_path} (defaults to
#'           \code{"fixed_effects"}).
#'     \item a data frame name available at \code{data_path} (defaults to \code{"data"}).
#'   }
#' @param remove Character scalar. Variable whose interaction terms should
#'   be considered for removal (passed to \code{fixed_effect_drops()}).
#'   Use \code{"all_interactions"} to consider all interactions.
#' @param new_random_effect Character scalar. Random-slope variable to add
#'   during refits (forwarded to \code{fixed_effect_drops()}).
#' @param alpha Numeric. Interactions with p-values \emph{greater than} \code{alpha}
#'   are considered non-significant and eligible for dropping.
#' @param model_env Optional environment to bind generated formulas. If \code{NULL},
#'   a new environment is created with \code{globalenv()} as parent (a warning is issued).
#' @param model_path Character vector of names describing where to find the model
#'   comparison history within each list element. Default \code{c("mc")}.
#'   (Reserved for future use; the current implementation uses \code{item$mc} directly.)
#' @param fes_path Character vector of names describing where to find the
#'   fixed-effects character vector within each list element. Default \code{"fixed_effects"}.
#' @param data_path Character vector of names describing where to find the \emph{name}
#'   (character string) of the data frame within each list element. Default \code{"data"}.
#'   The data frame is retrieved via \code{get()} in the parent frame.
#' @param name_path Character vector of names describing where to find a label
#'   for the entry. Default \code{"name"}. (Currently not used; reserved for logging prefixes.)
#' @param output_path Optional character string. Name of the list element
#'   under which to store the fixed-effect drop history for each model.
#'   Defaults to \code{"fed<remove>"} if \code{NULL}.

#' @details
#' For each element \code{i} of \code{model_list}:
#' \enumerate{
#'   \item The starting model is obtained with \code{final_model_mlm_comparison(item$mc)}.
#'   \item \code{fixed_effects} and the \code{data} object name are pulled using a
#'         simple nested accessor (via \code{fes_path} and \code{data_path}), then
#'         the data frame is resolved by \code{get()}.
#'   \item \code{\link{fixed_effect_drops}} is called with these inputs.
#'   \item The returned history is saved as \code{model_list[[i]]fedx1}.
#' }
#'
#' @return The input \code{model_list} with an additional element
#'   \code{$fed_x1} attached to each processed entry, containing the
#'   step-by-step history (class \code{"fixed_effect_drop_history"}).
#'
#' @examples
#' \dontrun{
#' # Assuming each entry has $mc (from mlm_comparison), $fixed_effects, and $data:
#' out <- run_fixed_effect_drops(
#'   model_list       = models_expanded,
#'   remove      = "x1",
#'   new_random_effect= "x1",
#'   alpha            = 0.10
#' )
#' # Inspect the fixed-effects drop history for the first entry
#' out[[1]]$fedx1
#' }
#'
#'@seealso \code{\link{log_fixed_effect_drop}}, \code{\link{fed_set_class}},
#' \code{\link{drop_nonsig_int_effects}}, \code{\link{build_fixed_term}},
#' \code{\link{build_formula}}, \code{\link{check_random_effects}},
#' \code{\link{get_random_effects}}, \code{\link{add_random_term}}
#' @export
run_fixed_effect_drops <- function(
    model_list,
    remove,
    new_random_effect,
    alpha,
    model_env = NULL,
    model_path = c("mc"),   # <--- single path of where to find the initial model
    fes_path = "fixed_effects",
    data_path = "data",
    name_path = "name",
    output_path = NULL
) {
  if (is.null(model_env)) {
    warning("The model environment was not pre-specified and was created.")
    model_env <- new.env(parent = globalenv())
  }

  if (is.null(output_path)) {
    output_path <- paste0("fed_", remove)
  }

  safe_pluck <- function(x, path) {
    Reduce(function(acc, nm) acc[[nm]], path, init = x)
  }

  for (i in seq_along(model_list)) {
    item <- model_list[[i]]

    model         <- final_model_mlm_comparison(model_list[[i]]$mc)
    fixed_effects <- safe_pluck(item, fes_path)
    data     <- safe_pluck(item, data_path)
    name_prefix   <- safe_pluck(item, name_path)

    data <- get(data, envir = parent.frame())

    fed_out <- fixed_effect_drops(
      model             = model,
      fixed_effects     = fixed_effects,
      remove            = remove,
      new_random_effect = new_random_effect,
      alpha             = alpha,
      model_env         = model_env,
      data              = data
    )



    model_list[[i]][[output_path]] <- fed_out
  }

  model_list
}


#' Print Summary of Fixed-Effect Drops
#'
#' Prints a clean textual summary of the iterative process used to remove
#' non-significant interaction terms from a mixed-effects model. Displays
#' the random-effects structure at each step, fixed-effects removed at each
#' iteration, and the fitting method used in the final model.
#'
#' @param x An object of class \code{"fixed_effect_drop_history"}, typically
#'   produced by \code{\link{fixed_effect_drops}}.
#' @param width Output width for line wrapping (default \code{getOption("width")}).
#' @param verbose Logical; whether to display the summary in the console.
#'   Set to \code{FALSE} for silent operation (default \code{TRUE}).
#' @param ... Additional arguments (ignored).
#'
#' @details
#' This function serves as the \code{print()} method for
#' \code{"fixed_effect_drop_history"} objects. When invoked, it prints
#' sections for:
#' \itemize{
#'   \item Random-effects structure in the initial, intermediate, and final models.
#'   \item Fixed-effects removed at each iteration.
#'   \item The fitting method (\code{"REML"} or \code{"ML"}) used in the final model.
#' }
#'
#' @return Invisibly returns a list with:
#'   \itemize{
#'     \item \code{$random_effects}  a list of random-effect structures by step.
#'     \item \code{$fixed_effects_removed_by_step} fixed effects removed at each step.
#'     \item \code{$final_method}  fitting method used in the final model.
#'   }
#'
#' @examples
#' \dontrun{
#' fed_hist <- fixed_effect_drops(model = fit, fixed_effects = fes,
#'                                remove = "x1", new_random_effect = "x1",
#'                                alpha = 0.10, data = df)
#' print(fed_hist)
#' }
#'
#' @seealso \code{\link{fixed_effect_drops}}, \code{\link{final_model_fed}}
#' @family print_helpers
#' @method print fixed_effect_drop_history
#' @export
print.fixed_effect_drop_history <- function(x, width = getOption("width"), verbose = TRUE, ...) {

  history <- x
  stopifnot(inherits(history, "fixed_effect_drop_history"), is.list(history))

  steps <- history
  # Robust integer coercion for mixed numeric types
  ord <- suppressWarnings(vapply(
    steps,
    function(s) if (!is.null(s$step)) as.integer(round(s$step)) else NA_integer_,
    integer(1)
  ))
  if (any(!is.na(ord))) steps <- steps[order(ord, na.last = TRUE)]

  step_names <- names(steps)
  if (is.null(step_names)) step_names <- paste0("step_", seq_along(steps))

  re_strings <- vector("list", length(steps))
  re_status  <- character(length(steps))
  fe_list    <- vector("list", length(steps))
  removed_by_step <- vector("list", length(steps))

  # Collect per-step info
  for (i in seq_along(steps)) {
    st <- steps[[i]]
    mdl <- st$model

    if (!inherits(mdl, "merMod")) {
      re_strings[[i]] <- "<no model>"
      re_status[i]    <- "unknown"
      fe_list[[i]]    <- .norm_vec(.get_fixed(st))
      next
    }
    rs <- .re_terms_from_model(mdl)
    re_strings[[i]] <- rs
    re_status[i]    <- .re_corr_status(rs)
    fe_list[[i]]    <- .norm_vec(.get_fixed(st))
  }

  # Determine removed-at-this-step
  ever_removed <- character(0)
  for (i in seq_along(steps)) {
    st <- steps[[i]]
    if (i == 1L) {
      raw <- if (!is.null(st$to_get_removed)) st$to_get_removed else character(0)
    } else {
      prev_fe <- fe_list[[i - 1L]]
      curr_fe <- fe_list[[i]]
      raw <- if (!is.null(st$to_get_removed) && length(st$to_get_removed))
        st$to_get_removed else .diff_removed(prev_fe, curr_fe)
    }
    raw <- .norm_vec(raw)
    new_only <- setdiff(raw, ever_removed)
    removed_by_step[[i]] <- new_only
    ever_removed <- unique(c(ever_removed, new_only))
  }

  # Identify initial/final
  initial_idx <- which(vapply(steps, function(s) identical(s$type, "initial_model"), logical(1)))
  final_idx   <- which(vapply(steps, function(s) identical(s$type, "final_model"),   logical(1)))
  if (!length(initial_idx)) initial_idx <- 1L
  if (!length(final_idx))   final_idx   <- length(steps)

  # Title header
  obj_name <- deparse1(substitute(x))
  obj_name <- gsub("^.*\\$", "", obj_name)
  obj_name <- sub("^mc_", "", obj_name)
  obj_name <- trimws(obj_name)
  title_line <- sprintf("Fixed Effects Non-Significant Drops by '%s'",
                        ifelse(nzchar(obj_name), obj_name, "variable"))

  cat("\n", strrep("=", nchar(title_line)), "\n", sep = "")
  cat(title_line, "\n", sep = "")
  cat(strrep("=", nchar(title_line)), "\n\n", sep = "")

  # ---- Print sections ----
  cat("Random effects (initial to final)\n")
  .wrapcat("  Initial: ", paste0(re_strings[[initial_idx]]), width)
  mid_ix <- setdiff(seq_along(steps), c(initial_idx, final_idx))
  if (length(mid_ix)) {
    for (i in mid_ix) {
      nm <- .step_label(step_names[i], steps[[i]])
      .wrapcat(sprintf("  %s: ", nm), re_strings[[i]], width)
    }
  }
  .wrapcat("  Final:   ", re_strings[[final_idx]], width)

  cat("\nFixed-effects removed (new removals at each step)\n")
  for (i in seq_along(steps)) {
    nm  <- .step_label(step_names[i], steps[[i]])
    rem <- removed_by_step[[i]]
    cat("  - ", nm, ":\n", sep = "")
    if (!length(rem)) cat("      (none)\n")
    else for (r in rem) cat("      \u2022 ", r, "\n", sep = "")
  }

  # Final method
  final_method <- NA_character_
  mi <- steps[[final_idx]]$model
  if (inherits(mi, "merMod")) {
    if (lme4::isREML(mi)) final_method <- "REML" else final_method <- "ML"
  }


  if (!is.na(final_method))
    cat(sprintf("\nFinal model fit method: %s\n", final_method))

  invisible(list(
    random_effects = list(
      initial = list(string = re_strings[[initial_idx]], status = re_status[initial_idx]),
      final   = list(string = re_strings[[final_idx]],   status = re_status[final_idx]),
      by_step = setNames(Map(function(s, st) list(string = s, status = st),
                             re_strings, as.list(re_status)),
                         step_names)
    ),
    fixed_effects_removed_by_step = setNames(removed_by_step, step_names),
    final_method = final_method
  ))
}


#' Extract the final fitted model from a fixed-effect drop history
#'
#' Retrieves the kept model from an object of class \code{"fixed_effect_drop_history"}
#' (as produced by \code{fixed_effect_drops()}). If a step explicitly marked
#' \code{type == "final_model"} exists, that step is used; otherwise, the model
#' from the last step in the history is returned.
#'
#' @param history A list with class \code{"fixed_effect_drop_history"} containing
#'   step entries with elements such as \code{$type} and \code{$model}.
#'
#' @return The final fitted model object (e.g., an \code{lmerMod} or \code{lm}
#'   object). An error is thrown if no model is found in the final step.
#'
#' @examples
#' \dontrun{
#' fit_hist <- fixed_effect_drops(model = fit, fixed_effects = fes,
#'                                    remove = "x1", new_random_effect = "x1",
#'                                    alpha = 0.10, data = df)
#' final_fit <- final_model_fed(fit_hist)
#' }
#'
#'@seealso \code{\link{log_fixed_effect_drop}}, \code{\link{fed_set_class}},
#' \code{\link{drop_nonsig_int_effects}}, \code{\link{build_fixed_term}},
#' \code{\link{build_formula}}, \code{\link{check_random_effects}},
#' \code{\link{get_random_effects}}, \code{\link{add_random_term}}
#'
#' @export
final_model_fed <- function(history) {
  stopifnot(inherits(history, "fixed_effect_drop_history"), is.list(history))

  steps <- history

  # Try to locate explicit "final_model"
  final_idx <- which(vapply(steps, function(s) identical(s$type, "final_model"), logical(1)))

  if (!length(final_idx)) {
    # if no explicit "final_model", just grab the last element
    final_idx <- length(steps)
  }

  final_step <- steps[[final_idx]]
  if (!is.null(final_step$model)) {
    return(final_step$model)
  } else {
    stop("No model object found in the final step.")
  }
}




