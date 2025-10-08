utils::globalVariables(c("formula", "model.frame", "terms"))


#' Test whether one set of terms is a subset of another
#'
#' Convenient wrapper to test if all elements of \code{a} are present in \code{b}.
#'
#' @param a,b Character vectors.
#'
#' @return Logical scalar, \code{TRUE} if \code{a} is a subset of \code{b}.
#' @keywords internal
#' @noRd
.is_subset <- function(a, b) all(a %in% b)  # a ⊆ b


#' Split an interaction term into sorted parts
#'
#' Converts a single interaction term into its component variable names, treating
#' \code{":"} and \code{"*"} equivalently, trimming whitespace, removing empties,
#' and sorting the parts.
#'
#' @param x A character scalar containing an interaction term.
#'
#' @return A character vector of the sorted variable names in the interaction.
#' @keywords internal
#' @noRd
.term_parts <- function(x) {
  strsplit(gsub(":", "*", x), "\\*", perl = TRUE)[[1]] |> trimws() |> (\(z) z[nzchar(z)])() |> sort()
}


#' Get dependent variable name from a fitted model
#'
#' Extracts the dependent variable (left-hand side) from a model's formula.
#'
#' @param model A fitted model object.
#'
#' @return A length-1 character vector with the dependent variable name.
#' @importFrom stats formula
#' @keywords lm_helper
#' @export
grab_dv <- function(model) {
  f <- formula(model)
  #the dependent variable is the first variable in the formula
  dv <- all.vars(f)[1]
  return(dv)
}


#' Extract fixed-effect term labels from a model or formula
#'
#' Returns the fixed-effect terms (as character labels) from a fitted model or
#' a model \code{formula}. If the input is an \code{lme4} mixed model
#' (\code{merMod}/\code{glmerMod}), random-effects bars are removed first so
#' only the fixed-effects portion is parsed.
#'
#' @param model A fitted model object (e.g., \code{lm}, \code{lmerMod}, \code{glmerMod})
#'   or a model \code{formula}.
#' @param include_intercept Logical; if \code{TRUE} and the model includes an
#'   intercept, prepend \code{"(Intercept)"} to the returned vector. Default \code{FALSE}.
#'
#' @return A character vector of fixed-effect term labels. Returns \code{character(0)}
#'   if no fixed-effect terms are present.
#'
#' @details
#' For mixed models, the function attempts to call \code{lme4::nobars()} on the
#' formula to drop any \code{(lhs | grp)} or \code{(lhs || grp)} terms prior to
#' extracting fixed-effect labels with \code{terms()}.
#'
#' @examples
#' \dontrun{
#' # From a formula
#' .grab_fixed_effects(y ~ x1 + x2 + x1:x2)
#'
#' # From an lm fit
#' fit_lm <- lm(mpg ~ wt + hp, data = mtcars)
#' .grab_fixed_effects(fit_lm)
#'
#' # From an lmer fit (random bars are ignored)
#' fit_lmer <- lme4::lmer(y ~ x + (1 + x | id), data = df)
#' .grab_fixed_effects(fit_lmer)
#'
#' # Include intercept label if present
#' .grab_fixed_effects(fit_lm, include_intercept = TRUE)
#' }
#'
#' @importFrom stats formula terms
#' @importFrom lme4 nobars
#' @keywords lm_helper
#' @export
grab_fixed_effects <- function(model, include_intercept = FALSE) {
  # Accept a model or a formula
  f <- if (inherits(model, "formula")) model else formula(model)

  # For lme4 models, drop random effects from the RHS
  # (keeps response ~ fixed-only)
  if (inherits(model, "merMod") || inherits(model, "glmerMod")) {
    # lme4::nobars() removes (..|..) and (..||..)
    f <- tryCatch(lme4::nobars(f), error = function(e) f)
  }

  # Build a terms object from the (fixed-only) formula
  tt <- terms(f)

  # Extract fixed-effect term labels (no intercept)
  fe_terms <- attr(tt, "term.labels")
  if (length(fe_terms) == 0L || is.null(fe_terms)) fe_terms <- character(0)

  # Optionally include the intercept if present in the model
  if (isTRUE(include_intercept) && !is.null(attr(tt, "intercept")) && attr(tt, "intercept") == 1) {
    fe_terms <- c("(Intercept)", fe_terms)
  }

  fe_terms
}



#' Get the grouping factor for the random intercept
#'
#' For an \code{lmerMod} fit, returns the name of the grouping variable corresponding
#' to the first random-effects block (typically the random intercept).
#'
#' @param model A fitted \code{lmerMod} object.
#'
#' @return A length-1 character string giving the grouping factor name.
#' @importFrom lme4 VarCorr
#' @importFrom stats model.frame
#' @keywords lm_helper
#' @export
grab_random_int_group <- function(model) {
  stopifnot(inherits(model, "lmerMod"))
  ri_name <- names(VarCorr(model))[1]
  ri_vals <- model.frame(model)[[ri_name]]

  output <- list(name = ri_name, values = ri_vals)
  return(output$name)
}



#' Grab non-significant interaction coefficients from a model
#'
#' Extracts interaction-term names from a fitted model whose p-values exceed
#' \code{alpha}. Optionally restricts to interactions involving a single variable.
#'
#' @param model A fitted model object (e.g., \code{lm}, \code{lmerMod}).
#' @param alpha Numeric threshold; interactions with p-values \emph{greater than}
#'   this are returned. Default \code{0.10}.
#' @param var Either \code{"all_interactions"} (default) to consider all interactions,
#'   or a single variable name to restrict to interactions involving that variable.
#'
#' @return A character vector of interaction-term names meeting the criterion.
#'   Returns \code{character(0)} if none are found.
#'
#' @details P-values are obtained from \code{summary(model)$coefficients}. The
#'   p-value column is located dynamically.
#' @keywords lm_helper
#' @export
grab_int_coefs <- function(model, alpha = .10, var = "all_interactions") {

  #> The purpose of this function is to take a variable and to grab all of its
  #> interaction terms in a model.
  #> Then it will return a character vector of all of the interaciton terms in
  #> the model

  #> variable = the base variable for which to grab all of its interaction terms
  #> model = the model to search in
  #> rename_vec = the nice_names name mapping vector for the rename.

  ms <- summary(model)

  # if(inherits(model, "merMod")) {}
  # if(inherits(model, "lm")) {}

  coefs <- as.data.frame(ms$coefficients)
  coefs$variable <- rownames(coefs)

  #grab name of p column
  p_col <- pcol(coefs)

  # default empty char vector (never return NULL)
  output <- character(0)

  #early out in case more than 1 variable is supplied.
  if(length(var) > 1) {

    warning(".grab_int_terms() was supplied more than a single variable to check
            for interaction terms; the first variable: ", var, " was used and
            others were ignored. Double check results.",
            "Ignored variables", var)

    var <- var[1]

    output <- coefs[grepl(var, coefs$variable) &
                      grepl("\\*|:", coefs$variable) &
                      coefs[[p_col]] > alpha,
                    "variable"]
  }


  #if only one variable is supplied, only grab that variable.
  if(var != "all_interactions" && length(var) == 1) { #grab the specific variable
    output <- coefs[grepl(var, coefs$variable) &
                      grepl("\\*|:", coefs$variable) &
                      coefs[[p_col]] > alpha,
                    "variable"]
  } else if (var == "all_interactions") { #grab all interaction terms
    output <- coefs[grepl("\\*|:", coefs$variable) &
                      coefs[[p_col]] > alpha,
                    "variable"]
  }

  return(output)
}



#' Split interaction term names into columns
#'
#' Splits each interaction term in \code{int_names} on \code{"*"} or \code{":"}
#' and returns a data frame with one column per factor in the widest interaction.
#' Shorter interactions are padded with \code{NA}.
#'
#' @param int_names Character vector of (possibly) interaction terms.
#'
#' @return A data frame whose rows correspond to \code{int_names}.
#' @examples
#' # int_strsplit(c("a*b", "a*b*c"))
#' @export
int_strsplit <- function(int_names) {

  #names = a character vector of interaction terms (and non-interaction terms
  #if desired)

  #> this function returns a dataframe with my interaction terms split up
  #so I can more easily rename them.

  # split on "*"
  s <- strsplit(int_names, "\\*|:")

  # find the maximum length (here: 3)
  k <- max(lengths(s))

  # pad each element to length k
  m <- t(vapply(s, function(x) {
    x <- trimws(x)          # remove extra spaces
    length(x) <- k          # pad with NA if needed
    x
  }, character(k)))

  m <- as.data.frame(m, stringsAsFactors = FALSE)

  return(m)

}


#' Join interaction parts back into term strings
#'
#' Collapses each row of a data frame of interaction components into a single
#' string, using \code{sep} between factors and omitting \code{NA}/empty entries.
#'
#' @param df A data frame with interaction component columns.
#' @param sep Separator between components (default \code{" * "}).
#'
#' @return A character vector of interaction-term strings.
#' @examples
#' # int_strjoin(data.frame(V1 = c("a","a"), V2 = c("b","c"), V3 = c(NA,"d")))
#' @export
int_strjoin <- function(df, sep = " * ") { #can provide ":" to sep to change
  #behavior of interactions
  apply(df, 1, function(row) {
    row <- trimws(row)
    row <- row[!is.na(row) & nzchar(row)]
    if (length(row)) paste(row, collapse = sep) else NA_character_
  })
}



#' Canonicalize an interaction term (order-agnostic)
#'
#' Normalizes an interaction string by treating \code{":"} and \code{"*"} as
#' equivalent, trimming whitespace, and sorting factor names alphabetically.
#'
#' @param x A character scalar interaction term.
#'
#' @return A normalized interaction string using \code{" * "} separators.
#' @keywords internal
#' @noRd
.norm_term <- function(x) {
  x <- gsub(":", "*", x)
  parts <- unlist(strsplit(x, "\\*", perl = TRUE))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  paste(sort(parts), collapse = " * ")
}


#' Build normalized keys for interaction terms
#'
#' Maps each original term to a normalized, order-agnostic key (via \code{.norm_term}).
#'
#' @param terms Character vector of terms.
#'
#' @return A named character vector: names are original terms, values are keys.
#' @keywords internal
#' @noRd
.build_keys <- function(terms) {
  setNames(vapply(terms, .norm_term, character(1)), terms)
}


#' Drop terms whose normalized keys match a removal set
#'
#' Keeps only those \code{terms} whose normalized keys are \emph{not} present
#' in the set derived from \code{to_drop}.
#'
#' @param terms Character vector of current terms.
#' @param to_drop Character vector of terms to remove (may have different ordering).
#'
#' @return A character vector of kept terms.
#' @keywords internal
#' @noRd
.drop_keys <- function(terms, to_drop) {
  if (is.null(to_drop) || !length(to_drop)) return(terms)
  keep_map <- .build_keys(terms)                 # names = original fes terms; values = keys
  drop_keys <- unique(unname(.build_keys(to_drop)))  # just the keys to drop
  names(keep_map)[!(unname(keep_map) %in% drop_keys)]
}


#' Detect interaction style used in fixed effects
#'
#' Heuristically determines whether interaction terms are supplied using
#' \code{":"} or \code{"*"}; mixed styles default to \code{":"}.
#'
#' @param fixed_effects Character vector of fixed-effect terms.
#'
#' @return A length-1 character, either \code{":"} or \code{"*"}.
#' @keywords internal
#' @noRd
.detect_style <- function(fixed_effects) {
  # mixed styles default to ":" (the stricter, colon-only intent)
  has_colon <- any(grepl(":", fixed_effects, fixed = TRUE))
  has_star  <- any(grepl("\\*", fixed_effects))
  if (has_colon && !has_star) return(":")
  if (has_star  && !has_colon) return("*")
  ":"  # mixed -> treat as colon-only
}


#' Drop non-significant interaction effects from fixed-effect terms
#'
#' Identifies interaction terms with p-values greater than \code{alpha} and
#' removes them from a supplied fixed-effects term vector, returning both the
#' removed terms and the updated set of terms.
#'
#' @param model A fitted model object (e.g., \code{lm}, \code{lmerMod}).
#' @param fixed_effects Character vector of fixed-effect terms as used in your model-building helpers.
#' @param alpha Numeric threshold; interactions with p-values \emph{greater than}
#'   this are considered for removal. Default \code{0.10}.
#' @param var Either \code{"all_interactions"} (default) to evaluate all interactions,
#'   or a single variable name to restrict to interactions involving that variable.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{variable}}{The value of \code{var} used.}
#'   \item{\code{full_terms}}{Original \code{fixed_effects}.}
#'   \item{\code{removed_terms}}{Character vector of removed interactions (or \code{NULL}).}
#'   \item{\code{new_model_terms}}{Updated fixed-effect terms after removal.}
#' }
#' An \emph{attribute} \code{"interaction_style"} stores the detected style (\code{":"} or \code{"*"}).
#'
#' @examples
#' \dontrun{
#' info <- drop_nonsig_int_effects(model = fit, fixed_effects = fes, alpha = 0.10)
#' info$new_model_terms
#' }
#' @export
drop_nonsig_int_effects <- function(
    model, #model where terms come from
    fixed_effects, #fixed effects character vector
    alpha = .10,
    #don't need to specify if I want to look at all interactions
    var = "all_interactions"
    #if I only want to not include interactions with one variable, specify that
    #variable here. Its passed to .grab_int_coefs
) {

  #> NOTE: I'm not sure what was subtly wrong with being able to use the below
  #> code so that I could optionally not supply fixed effects, but it was not
  #> working properly.
  #if I don't provide the character vector of fixed effects, grab from model.
  # if(is.null(fixed_effects)) {
  #   fixed_effects <- attr(terms(model), "term.labels")
  #   fixed_effects <- gsub(":", " * ", fixed_effects)
  # }

  #determine how interactions were supplied to feed into how output is formed
  style <- .detect_style(fixed_effects)

  #grabs the specific terms where the variable is interacting with other variables
  int_terms <- grab_int_coefs(model = model,
                               alpha = alpha,
                               var = var)

  # If there are no interaction terms to consider, exit early
  if (is.null(int_terms) || length(int_terms) == 0) {
    return(list(
      variable        = var,
      full_terms      = fixed_effects,
      removed_terms   = NULL,
      new_model_terms = fixed_effects
    ))
  }

  #splits the terms so they are in separate columns
  split_terms <- int_strsplit(int_terms)
  #rejoins those with "*" in-between so they are the same as my fixed effects
  #terms
  remove_terms <- int_strjoin(split_terms, sep = style)

  #> Assign a null status to remove_terms if there are no variables left
  if (length(remove_terms) == 0) {remove_terms <- NULL}

  keep_terms <- .drop_keys(fixed_effects, remove_terms)


  output <- list(
    variable = var,
    full_terms = fixed_effects,
    removed_terms = remove_terms,
    new_model_terms = keep_terms
  )
  # add the style into the returned object for downstream logic
  attr(output, "interaction_style") <- style

  return(output)
}


#' Recursively flatten a language object separated by '+'
#'
#' Recursively descends an expression and returns a list of leaf expressions
#' separated by the \code{+} operator.
#'
#' @param expr A language object (e.g., the LHS inside a random-effects bar).
#'
#' @return A list of leaf expressions.
#' @keywords internal
#' @noRd
.flatten_plus <- function(expr) {
  # Recurse only on '+' calls; symbols/numbers return as a leaf
  if (is.call(expr) && identical(expr[[1]], as.name("+"))) {
    c(.flatten_plus(expr[[2]]), .flatten_plus(expr[[3]]))
  } else {
    list(expr)
  }
}


#' Extract left-hand-side tokens from a random-effects term
#'
#' Converts the left-hand side of a random-effects bar term into trimmed symbol
#' strings (e.g., \code{"1"}, \code{"0"}, \code{"x"}, \code{"z"}).
#'
#' @param lhs A language object representing the left side of a random-effects bar.
#'
#' @return A character vector of tokens.
#' @keywords internal
#' @noRd
.lhs_terms <- function(lhs) {
  parts <- .flatten_plus(lhs)
  syms  <- vapply(parts, function(p) paste(deparse(p), collapse = " "), character(1))
  trimws(syms)
}



#' Inspect random-effects structure in a formula or lmer model
#'
#' Parses a mixed-model formula (or \code{lmerMod} fit) to summarize whether any
#' random effects are present, whether slopes/intercept are included, and whether
#' terms are correlated or uncorrelated, returning a detailed data frame.
#'
#' @param model A fitted \code{lmerMod} object or a model \code{formula}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{any_random_effects}}{Logical.}
#'   \item{\code{any_intercept}}{Logical; whether any random intercept is present.}
#'   \item{\code{any_random_slopes}}{Logical; whether any random slopes are present.}
#'   \item{\code{all_uncorrelated}}{Logical; \code{TRUE} if all RE blocks are uncorrelated (or unary).}
#'   \item{\code{details}}{Data frame with one row per RE bar (group, operator, lhs, etc.).}
#' }
#'
#' @examples
#' \dontrun{
#' re <- check_random_effects(lme4::lmer(y ~ x + (1 + x | id), data = df))
#' re$details
#' }
#' @importFrom lme4 findbars
#' @importFrom stats formula model.frame
#' @export
check_random_effects <- function(model) {

  #>NOTE: ChatGPT wrote this one for me and it works nicely. I need to document
  #> it and perhaps touch it up a bit for my liking.

  if (inherits(model, "merMod")) {
    fml <- formula(model)
  } else if (inherits(model, "formula")) {
    fml <- model
  } else stop("Provide an lmer model or a formula.")

  bars <- lme4::findbars(fml)  # list of (lhs | grp) or (lhs || grp)
  if (length(bars) == 0) {
    return(list(
      any_random_effects = FALSE,
      any_random_slopes  = FALSE,
      all_uncorrelated   = TRUE,
      details = data.frame()
    ))
  }

  details <- do.call(rbind, lapply(bars, function(b) {
    op   <- as.character(b[[1]])                # "|" or "||"
    lhs  <- b[[2]]
    grp  <- paste(deparse(b[[3]]), collapse=" ")
    lhs_strs <- .lhs_terms(lhs)                 # tokens like "1","0","x","z"
    has_intercept <- any(lhs_strs == "1")
    # anything other than 0/1 counts as a slope spec
    slopes <- setdiff(lhs_strs, c("0","1"))
    has_slopes <- length(slopes) > 0
    n_components <- length(setdiff(lhs_strs, "0"))  # count ignoring 0 terms

    data.frame(
      group         = grp,
      operator      = op,
      lhs           = paste(lhs_strs, collapse = " + "),
      has_intercept = has_intercept,
      has_slopes    = has_slopes,
      n_components  = n_components,
      term_uncorrelated = (op == "||") || (n_components <= 1),
      stringsAsFactors = FALSE
    )
  }))

  any_rand <- nrow(details) > 0
  any_intercept <- any(details$has_intercept)
  any_slopes <- any(details$has_slopes)
  all_uncor <- all(details$term_uncorrelated)

  list(
    any_random_effects = any_rand,
    any_intercept = any_intercept,
    any_random_slopes  = any_slopes,         # beyond the intercept
    all_uncorrelated   = all_uncor,          # TRUE if no correlated blocks
    details            = details
  )
}



#' Get the unique random-effect terms from \code{check_random_effects()}
#'
#' Extracts unique left-hand-side terms from the \code{details} table returned
#' by \code{\link{check_random_effects}}, normalized by splitting on \code{"+"},
#' trimming, and removing \code{"0"} and empty strings. The intercept is returned
#' as \code{"1"}.
#'
#' @param re_info A list produced by \code{\link{check_random_effects}}.
#'
#' @return A character vector of random-effect terms (e.g., \code{c("1", "x", "z")}).
#' @export
get_random_effects <- function(re_info) {

  #This is a super useful helper function which grabs the unique random effect
  #terms by looking at the object output from check_random_effects.
  #This function outputs a character vector of the length of the number of
  #random effects.
  #> e.g., c("1", "Zprog_ww", "Zestr_ww")

  stopifnot("details" %in% names(re_info))

  lhs <- as.character(re_info$details$lhs)

  # Replace intercept indicator with "1"
  lhs[lhs == "(Intercept)" | lhs == ""] <- "1"

  # Keep unique terms, in order of appearance
  lhs <- unique(lhs)


  lhs <- strsplit(lhs, "\\+")
  lhs <- trimws(unlist(lhs))

  # remove empty strings and "0"
  lhs <- lhs[lhs != ""]
  lhs <- lhs[lhs != "0"]

  return(lhs)
}



#' Recommend random-effects correlation/intercept settings from structure
#'
#' Given \code{check_random_effects()} output, returns suggested values for
#' \code{covary} and \code{intercept} to be used with your random-term builder,
#' along with a status label.
#'
#' @param re_info A list produced by \code{\link{check_random_effects}}.
#'
#' @return A list with elements \code{covary} (logical), \code{intercept} (logical),
#'   and \code{status} (one of \code{"only_intercept"}, \code{"only_slope"},
#'   \code{"uncorrelated"}, \code{"correlated"}, \code{"empty"}).
#' @keywords internal
#' @noRd
.random_structure_action <- function(re_info) {
  has_slopes <- isTRUE(re_info$any_random_slopes)
  has_int    <- isTRUE(re_info$any_intercept)
  all_uncorr <- isTRUE(re_info$all_uncorrelated)

  status <- if (has_slopes && !has_int) {
    "only_slope"
  } else if (!has_slopes && has_int) {
    "only_intercept"
  } else if (has_slopes && all_uncorr) {
    "uncorrelated"
  } else if (has_slopes) {
    "correlated"
  } else {
    "empty"
  }

  actions <- list(
    only_intercept = list(covary = TRUE,  intercept = TRUE),
    only_slope     = list(covary = TRUE,  intercept = FALSE),
    uncorrelated   = list(covary = FALSE, intercept = TRUE),
    correlated     = list(covary = TRUE,  intercept = TRUE),
    empty          = list(covary = FALSE, intercept = TRUE) # harmless default
  )

  c(actions[[status]], list(status = status))
}



#' Add a random-effect term consistent with the model's current structure
#'
#' Determines the current random-effects structure of \code{model} and returns a
#' random-effects term string (via \code{build_random_term}) that includes the
#' supplied \code{new_random_effect} while preserving (or appropriately setting)
#' correlation and intercept behavior.
#'
#' @param new_random_effect Character scalar; the new random slope to add.
#' @param model A fitted model whose random-effects structure will be inspected.
#' @param group Character scalar; grouping variable name.
#' @param data Optional data frame for variable-existence checks used by
#'   \code{build_random_term}.
#'
#' @return A character string representing the updated random-effects term.
#'
#' @examples
#' \dontrun{
#' add_random_term("x2", model = fit, group = "id", data = df)
#' }
#' @seealso \code{\link{check_random_effects}}, \code{\link{get_random_effects}},
#'   \code{\link{build_random_term}}
#' @export
add_random_term <- function(new_random_effect, #new term
                            model, #model to grab final random terms from
                            group,
                            data = NULL) {

  #> HERE I NEED TO MAKE SOME IF ELSE STATEMENTS TO DETERMINE HOW TO ADD IN THE
  #> RANDOM EFFECT
  #> 3 OPTIONS:
  #> ONLY RANDOM INTERCEPT -- THEN MAKE CORRELATED RANDOM EFFECTS MODEL
  #> UNCORRELATED RANDOM EFFECTS -- THEN ADD IN NEW TERM AS UNCORRELATED WITH OTHERS
  #> CORRELATED RANDOM EFFECTS -- THEN ADD IN NEW TERM AS CORRELATED WITH OTHERS

  # what does the model currently have?
  re_info <- check_random_effects(formula(model))

  #statements to check if there are random slopes, and if those are correlated
  #or uncorrelated
  # if (re_status$any_random_slopes == FALSE &&
  #     re_status$all_uncorrelated == TRUE &&
  #     re_status$details$n_components == 1) {re_status <- "only_intercept"}
  #
  # if(re_status$any_random_slopes == TRUE &&
  #    re_status$all_uncorrelated == FALSE &&
  #    re_status$details$n_components > 1) {re_status <- "correlated"}
  #
  # if(re_status$any_random_slopes == TRUE &&
  #    re_status$all_uncorrelated == TRUE) {re_status <- "uncorrelated"}
  #
  # 2) Derive one of: "only_intercept", "uncorrelated", "correlated"
  has_slopes     <- isTRUE(re_info$any_random_slopes)
  has_int        <- isTRUE(re_info$any_intercept)
  all_uncorr     <- isTRUE(re_info$all_uncorrelated)
  status <- if (has_slopes && !has_int) {
    "only_slope"
  } else if (!has_slopes && has_int) {
    "only_intercept"
  } else if (has_slopes && all_uncorr) {
    "uncorrelated"
  } else if (has_slopes) {
    "correlated"
  } else {
    NA_character_  # fallback if neither
  }

  # look-up table: map status -> (covary, intercept)
  actions <- list(
    only_intercept = list(covary = TRUE,  intercept = TRUE),   # correlated with intercept
    only_slope     = list(covary = TRUE,  intercept = FALSE),  # correlated with other random slope
    uncorrelated   = list(covary = FALSE, intercept = TRUE),   # add as uncorrelated slope
    correlated     = list(covary = TRUE,  intercept = TRUE)    # keep correlated structure
  )

  # get the rule (default to correlated if unknown)
  act <- actions[[status]]
  if (is.null(act)) act <- actions$correlated

  # grab the random effects from the check_random_effects information
  random_effects <- get_random_effects(re_info)

  res <- c(random_effects, new_random_effect)

  new_random_term <- build_random_term(
    random_effects = res,
    group = group,
    covary = act$covary,
    intercept = act$intercept,
    data = data
  )
  return(new_random_term)
}


#' Drop a main-effect from a fixed-effects term vector
#'
#' Removes the specified effect from a character vector of fixed-effect terms.
#'
#' @param fixed_effects Character vector of fixed-effect terms.
#' @param drop_effect Character scalar; the term to remove.
#'
#' @return A character vector with \code{drop_effect} removed.
#' @export
drop_main_effect <- function(fixed_effects, drop_effect) {

  output <- fixed_effects[!fixed_effects %in% drop_effect]

  return(output)

}


#' Check the significance of a specific coefficient by name
#'
#' Retrieves the p-value for a named coefficient in \code{summary(model)$coefficients}.
#' If \code{sig_value} is provided, returns a logical indicating whether the p-value
#' is less than \code{sig_value}.
#'
#' @param model A fitted model object.
#' @param var Character scalar; coefficient/row name to look up.
#' @param sig_value Optional numeric; if provided, return \code{p < sig_value}.
#'
#' @return If \code{sig_value} is \code{NULL}, a numeric p-value; otherwise a logical.
#' @keywords internal
#' @noRd
check_significance <- function(model, var, sig_value = NULL) {

  #grab model summary and turn it into a data frame with the variables as a new
  #column name to work with
  ms <- summary(model)
  coefs <- as.data.frame(ms$coefficients)
  coefs$variable <- rownames(coefs)

  p_col <- pcol(coefs)

  #grab the p-value
  output <- coefs[coefs$variable == var, p_col]

  #if I provided a significance value, then compare the significant value to the
  #value obtained and returned true/false.
  #saves a step of running the check to see if the p-value is less than specific
  #value
  if(!is.null(sig_value)) {

    output <- output < sig_value

  }

  return(output)
}


#' Protect two-way interactions that are part of three-way interactions
#'
#' Given the result of \code{drop_nonsig_int_effects()}, prevents removal of
#' two-way interactions that belong to any existing three-way interaction, to
#' avoid infinite loops during iterative model simplification.
#'
#' @param drop_info A list returned by \code{\link{drop_nonsig_int_effects}},
#'   with attributes including \code{"interaction_style"}.
#'
#' @return A possibly reduced character vector of \code{removed_terms}, or
#'   \code{NULL} if none remain after protection.
#' @keywords internal
#' @noRd
.protect2way <- function(drop_info) {

  #> This function takes the input object of drop_nonsig_int_effects and
  #> then protects all of the 2-way interactions from being removed if they are
  #> a part of a higher-order 3-way interaction.
  #> This is necessary when I'm looping because I want to exhaust the items in
  #> the removed_terms element, but if the new model ran always includes the
  #> lower order interaction terms (because they are non-sig), then it will
  #> endlessly loop.
  #> This function returns the updated "removed_terms" element of the drop_info
  #> object from the drop_nonsig_int_effects

  #> If I have supplied ":" interaction terms, this function is unecessary and I
  #> can just output the original object removed objects
  # at top of .protect2way
  style <- attr(drop_info, "interaction_style")

  # If user supplied ":" (or mixed you want to treat as ":"), skip protection
  if (isTRUE(identical(style, ":"))) {
    return(drop_info$removed_terms)
  }

  keep <- drop_info$new_model_terms
  drop <- drop_info$removed_terms

  #split up the interactions into separate columns to work with them
  keep <- int_strsplit(keep)

  # if there aren't at least 3 columns, there are no 3-way interactions to protect
  if (ncol(keep) < 3) return(drop) #return original drop list

  #keep only third order interactions
  third_order <- keep[!is.na(keep[, 3]), ]

  #create all unique potential combinations of third_order interactions broken
  #into two_way interactions (in case order of variables has changed)
  pairs_from_third <- c(
    paste(third_order$V1, third_order$V2, sep = " * "),
    paste(third_order$V1, third_order$V3, sep = " * "),
    paste(third_order$V2, third_order$V3, sep = " * ")
  )
  #grab unique pairs
  pairs_from_third <- unique(pairs_from_third)
  #removing any potential missing items created
  pairs_from_third <- pairs_from_third[!is.na(pairs_from_third)]

  #true/false vector creation to identify the 2-way interactions in the removal
  #terms
  is_2way <- grepl("\\*", drop) & (sapply(gregexpr("\\*", drop), function(z) sum(z > 0)) == 1)


  # Build lookup keys (this protects from strange naming patterns by calling
  #.norm terms within it)
  third_keys   <- .build_keys(pairs_from_third)
  removed_keys <- .build_keys(drop_info$removed_terms)

  # Compare by keys
  protect_keys <- intersect(unname(removed_keys), unname(third_keys))

  # map the DROP TERMS into their normalized form for comparison
  drop_norm <- unname(removed_keys[drop])  # normalized strings aligned to 'drop'

  keep_mask <- !(is_2way & drop_norm %in% protect_keys)

  removed_items <- drop[keep_mask]

  # return NULL if none left (to help stop the while-loop cleanly)
  if (length(removed_items) == 0) return(NULL)
  return(removed_items)

}


#' Miscellaneous summaries for an \code{lmer} model
#'
#' Computes a small set of convenience diagnostics from a fitted mixed-effects
#' model, including the number of groups, sample size, random-effects variance
#' components, and both adjusted and unadjusted ICCs.
#'
#' @param lmer_model A fitted \code{lmerMod} object (e.g., from \code{lmerTest::lmer()}
#'   or \code{lme4::lmer()}).
#'
#' @details
#' The ICCs are computed as:
#' \itemize{
#'   \item Adjusted ICC: \eqn{( \text{total variance} - \text{residual variance} ) / \text{total variance}}
#'   \item Unadjusted ICC: \eqn{\text{intercept variance} / (\text{intercept variance} + \text{residual variance})}
#' }
#' If no random intercept variance is found, both ICCs are set to \code{NA} and a warning is issued.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{\code{ngrps}}{Number of levels (per grouping factor) as reported by \code{summary()}.}
#'   \item{\code{sample_size}}{Number of observations used in the fit (length of residuals).}
#'   \item{\code{ICC}}{A list with \code{ICC_adj} and \code{ICC_unadj}, each carrying a descriptive note.}
#'   \item{\code{random_effects}}{The variance–covariance components (\code{$varcor}) from \code{summary()}.}
#' }
#'
#' @examples
#' \dontrun{
#' fit <- lmerTest::lmer(y ~ x + (1 | id), data = df)
#' misc <- misc_lmer(fit)
#' str(misc)
#' }
#'
#' @importFrom lme4 VarCorr
#' @export
misc_lmer <- function(lmer_model) {

  #start a list to store information:
  misc_output <- vector("list")

  #rename model and grab summary for ease of grabbing information
  mr <- lmer_model
  ms <- summary(lmer_model)


  model_re <- as.data.frame(VarCorr(mr))

  ### Calculate adjusted and unadjusted ICC:

  # Total variance
  total_var <- sum(model_re$vcov)
  # Residual variance
  resid <- model_re$vcov[model_re$grp == "Residual"][1]
  # Random intercept variance
  intercept <- model_re$vcov[
    model_re$var1 == "(Intercept)" & is.na(model_re$var2)
  ][1]

  # Adjusted ICC
  adj_icc <- (total_var - resid) / total_var
  attr(adj_icc, "note") <- "Adjusted ICC = (total variance - residual variance) / total variance"
  # Unadjusted ICC
  unadj_icc <- intercept / (intercept + resid)
  attr(unadj_icc, "note") <- "Unadjusted ICC = intercept variance / (intercept variance + residual variance)"

  #If the intercept term was missing, flash a warning and don't calculate ICC
  #> This is a good thing to do because it is very unlikely that I will run models
  #> without an intercept and if I do there is a decent chance that was an error
  #> produced somewhere else.
  if(is.na(intercept)) {
    adj_icc <- NA
    unadjc_icc <- NA
    attr(adj_icc, "note") <- "No random intercept was found in model; ICC was not
    calculated"
    attr(unadj_icc, "note") <- "No random intercept was found in model; ICC was not
    calculated"

    warning("misc_lmer() found no random intercept term in the lmer model and the
    ICC was not computed. Check model if a random intercept was expected.")
  }

  #> number of groups of the grouping variable (if family_id = number of differnt
  #> families, if participant_id = number of people)
  misc_output$ngrps <- ms$ngrps
  misc_output$sample_size <- length(ms$residuals) #length of residuals is valid cases
  misc_output$ICC <- list(
    ICC_adj = adj_icc,
    ICC_unadj = unadj_icc
  )
  misc_output$random_effects <- ms$varcor

  return(misc_output)
}


#' Miscellaneous summaries for an \code{lm} model
#'
#' Extracts a compact set of diagnostics from a fitted linear model, including
#' sample size, \eqn{R^2}, adjusted \eqn{R^2}, F statistic, and numerator/denominator
#' degrees of freedom.
#'
#' @param lm_model A fitted \code{lm} object.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{\code{sample_size}}{Number of observations used in the fit.}
#'   \item{\code{R_squared}}{\eqn{R^2} from \code{summary()}.}
#'   \item{\code{Adj_R_squared}}{Adjusted \eqn{R^2} from \code{summary()}.}
#'   \item{\code{f_statistic}}{Model F-statistic value.}
#'   \item{\code{df_num}}{Numerator degrees of freedom.}
#'   \item{\code{df_den}}{Denominator degrees of freedom.}
#' }
#'
#' @examples
#' \dontrun{
#' fit <- lm(y ~ x1 + x2, data = mtcars)
#' misc <- misc_lm(fit)
#' str(misc)
#' }
#' @export
misc_lm <- function(lm_model) {

  #start a list to store information:
  misc_output <- vector("list")

  #rename model and grab summary for ease of grabbing information
  mr <- lm_model
  ms <- summary(lm_model)


  misc_output$sample_size <- length(mr$residuals)
  misc_output$R_squared <- ms$r.squared
  misc_output$Adj_R_squared <- ms$adj.r.squared
  misc_output$f_statistic <- ms$fstatistic["value"]
  misc_output$df_num <- ms$fstatistic["numdf"]
  misc_output$df_den <- ms$fstatistic["dendf"]

  return(misc_output)

}


#' Append miscellaneous summaries to each element of a model list
#'
#' For each entry in \code{model_list}, extracts the fitted model object and
#' computes a compact set of diagnostics using \code{\link{misc_lmer}} (for
#' mixed models) or \code{\link{misc_lm}} (for linear models). The results are
#' attached to each element under \code{$misc_output}.
#'
#' @param model_list A list whose elements contain fitted model objects. This
#'   function looks for:
#'   \itemize{
#'     \item \code{$mr} — the fitted model (preferred), or
#'     \item \code{$mc$final_model} — the final model inside a comparison history.
#'   }
#'
#' @return The input \code{model_list}, with a new element \code{$misc_output}
#'   added to each component that contains a recognized model type.
#'
#' @examples
#' \dontrun{
#' # Suppose each entry has $mr (lm or lmerMod)
#' out <- grab_misc(model_list)
#' out[[1]]$misc_output
#' }
#'
#' @seealso \code{\link{misc_lmer}}, \code{\link{misc_lm}}
#' @export
grab_misc <- function(model_list) {

  #model is an lmer or lm model

  for(i in seq_along(model_list)) {


    model <- model_list[[i]]$mr #specific model
    # in case I only have the final model stored here:
    if(is.null(model)) model <- model_list[[i]]$mc$final_model

    if(inherits(model, "merMod")) {
      output <- misc_lmer(model)
      #misc_lmer returns a list of the most important model information I can
      #append to my model list
      model_list[[i]]$misc_output <- output
    }

    if(inherits(model, "lm")) {
      output <- misc_lm(model)
      #misc_lm returns a list of the most important model information I can
      #append to my model list
      model_list[[i]]$misc_output <- output
    }

  } #end of for loop

  return(model_list)
}


