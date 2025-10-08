utils::globalVariables(c("nobs", "logLik", "AIC", "BIC"))




#' Initialize an Empty Model-Comparison History
#'
#' Creates a fresh list to be used as the step-by-step log (history) of
#' \code{\link{mlm_comparison}} decisions.
#'
#' @return An empty list of length 0 intended to hold named step entries.
#'
#' @examples
#' H <- init_history()
#' length(H)  # 0
#'
#' @keywords internal
#' @export
init_history <- function() {list()}


#' Internal null-coalescing operator
#'
#' Returns \code{b} when \code{a} is \code{NULL}, otherwise returns \code{a}.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a


#' Normalize anova() output to a data.frame
#'
#' Reorders/renames common columns (AIC, BIC, logLik, -2LL, Chisq, Df, P_Chisq).
#' Preserves the original heading in an attribute.
#' @keywords internal
#' @noRd
.anova_to_df <- function(x) {
  if (is.null(x)) return(NULL)
  rn  <- attr(x, "row.names")
  hdr <- attr(x, "heading")
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE, check.names = FALSE)
  if (!is.null(rn) && length(rn) == nrow(df)) rownames(df) <- rn
  rename_map <- c("-2*log(L)"="-2LL","X.2.log.L."="-2LL","Pr(>Chisq)"="P_Chisq","Pr..Chisq."="P_Chisq")
  for (old in names(rename_map)) if (old %in% names(df)) names(df)[names(df)==old] <- rename_map[[old]]
  order_cols <- c("npar","AIC","BIC","logLik","-2LL","Chisq","Df","P_Chisq")
  keep <- intersect(order_cols, names(df))
  df <- df[, c(keep, setdiff(names(df), keep)), drop = FALSE]
  attr(df, "heading") <- hdr
  df
}

#' Locate the p-value column in a coefficient table
#'
#' Finds the name of the p-value column in a coefficient table such as
#' \code{summary(model)$coefficients}. Works for both matrices and data frames
#' and matches headers like \code{"Pr(>|t|)"} or \code{"Pr(>|z|)"}.
#'
#' @param cf A matrix or data frame of coefficients with column names.
#'
#' @return A length-1 character string giving the p-value column name.
#' @keywords formatting
#' @export
pcol <- function(cf) {
  cn <- colnames(cf)              # works for matrix or data.frame
  ix <- grep("^Pr\\(>.*\\)$", cn) # matches "Pr(>|t|)" or "Pr(>|z|)"
  if (!length(ix)) stop("No p-value column found")
  cn[ix[1]]
}


#' Format p-values
#'
#' Formats p-values with fixed digits and uses the conventional
#' threshold display \code{"< 0.001"} for very small values.
#'
#' @param p Numeric vector of p-values.
#' @param digits Integer scalar; number of digits to print after the decimal
#'   for p-values \eqn{\ge} 0.001. Default is 3.
#'
#' @return A character vector the same length as \code{p}.
#'
#' @examples
#' format_p(c(NA, 0.25, 0.0499, 0.001, 0.0007))
#'
#' @keywords formatting
#' @export
format_p <- function(p, digits = 3) {
  ifelse(
    is.na(p), NA_character_,
    ifelse(p < .001, "< 0.001", formatC(p, digits = digits, format = "f"))
  )
}


#' Significance stars
#'
#' Returns conventional significance markers based on p-value:
#' \code{""}, \code{"."}, \code{"*"}, \code{"**"}, or \code{"***"}.
#'
#' @param p Numeric vector of p-values.
#'
#' @return A character vector the same length as \code{p} containing the markers.
#'
#' @examples
#' sig_stars(c(NA, 0.20, 0.08, 0.049, 0.009, 0.0004))
#'
#' @keywords formatting
#' @export
sig_stars <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(p < .001, "***",
           ifelse(p < .01,  "**",
                  ifelse(p < .05,  "*",
                         ifelse(p < .1, ".", "")
                  )
           )
    )
  )
}


#' Extract random-effects blocks from a fitted model formula
#'
#' Parses the right-hand side of a model's formula and returns a compact
#' string of the random-effects terms found (e.g., \code{"(1 + x | id) + (1 | grp)"}).
#' If no random effects are present, returns \code{"<no random effects?>"}.
#'
#' @param model A fitted model object with a \code{formula()} (e.g., \code{lmerMod}, \code{lm}).
#'
#' @return A length-1 character string summarizing random-effects terms.
#'
#' @examples
#' \dontrun{
#' .re_terms_from_model(lme4::lmer(y ~ x + (1 + x | id), data = df))
#' }
#'
#' @importFrom stats formula
#' @family print_helpers
#' @export
.re_terms_from_model <- function(model) {
  f <- formula(model)
  rhs <- paste0(deparse(f[[3]]), collapse = "")
  m <- gregexpr("\\([^()]*\\|[^()]*\\)", rhs, perl = TRUE)
  parts <- regmatches(rhs, m)[[1]]
  parts <- trimws(gsub("\\s+", " ", parts))
  if (!length(parts)) return("<no random effects?>")
  paste(parts, collapse = " + ")
}



#' Classify random-effects correlation status from a string
#'
#' Given a random-effects summary string (as produced by \code{.re_terms_from_model()}),
#' determines whether the structure is \code{"correlated"}, \code{"uncorrelated"}, or \code{"none"}.
#'
#' @param re_string Character scalar describing random-effects blocks.
#'
#' @return One of \code{"correlated"}, \code{"uncorrelated"}, or \code{"none"}.
#'
#' @examples
#' \dontrun{
#' .re_corr_status("(1 + x | id)")
#' .re_corr_status("(1 | id)")
#' .re_corr_status("<no random effects?>")
#'  }
#' @family print_helpers
#' @export
.re_corr_status <- function(re_string) {
  if (identical(re_string, "<no random effects?>")) return("none")
  blocks <- strsplit(re_string, "\\+\\s(?=\\()", perl = TRUE)[[1]]
  blocks <- trimws(blocks)
  has_multi <- vapply(blocks, function(b) {
    inner <- sub("^\\(", "", sub("\\)$", "", b))
    left  <- strsplit(inner, "\\|", perl = TRUE)[[1]][1]
    left  <- trimws(left)
    sum(trimws(unlist(strsplit(left, "\\+"))) != "") > 1
  }, logical(1))
  if (any(has_multi)) "correlated" else "uncorrelated"
}


#' Compute which fixed-effect terms were removed between steps
#'
#' Returns the set difference \code{prev \\ curr} after trimming whitespace,
#' handling empty inputs robustly.
#'
#' @param prev,curr Character vectors of fixed-effect terms (previous and current).
#'
#' @return A character vector of terms present in \code{prev} but not in \code{curr}.
#'
#' @examples
#' \dontrun{
#' .diff_removed(c("a","b","a:b"), c("a","b"))
#'}
#' @family print_helpers
#' @export
.diff_removed <- function(prev, curr) {
  prev <- if (length(prev)) trimws(prev) else character(0)
  curr <- if (length(curr)) trimws(curr) else character(0)
  setdiff(prev, curr)
}


#' Safely extract fixed-effect terms from a history step
#'
#' Retrieves \code{step$fixed_effects}, returning \code{character(0)} if missing or \code{NULL}.
#'
#' @param step A list-like history step entry containing \code{$fixed_effects}.
#'
#' @return A character vector (possibly length 0).
#'
#' @examples
#' \dontrun{
#' .get_fixed(list(fixed_effects = c("x","z","x:z")))
#' .get_fixed(list())
#'}
#' @family print_helpers
#' @export
.get_fixed <- function(step) {
  x <- step$fixed_effects
  if (is.null(x)) character(0) else x
}


#' Compose a human-readable step label
#'
#' Builds a label of the form \code{"<name> (step %02d, <type>)"} when
#' \code{$step} and \code{$type} are available; otherwise falls back to \code{step_list_name}.
#'
#' @param step_list_name Character label for the step (e.g., list name).
#' @param step A list-like step entry with fields \code{$step} and \code{$type}.
#'
#' @return A character label.
#'
#' @examples
#' \dontrun{
#' .step_label("s01_drop", list(step = 1, type = "edited_model"))
#'  }
#' @family print_helpers
#' @export
.step_label <- function(step_list_name, step) {
  if (!is.null(step$type) && !is.null(step$step)) {
    sprintf("%s (step %02d, %s)", step_list_name, as.integer(round(step$step)), step$type)
  } else {
    step_list_name
  }
}


#' Summarize basic model fit information
#'
#' Returns a one-row data frame with \code{nobs}, \code{logLik}, \code{AIC}, \code{BIC},
#' and the fit method (\code{"ML"} or \code{"REML"} when available).
#'
#' @param model A fitted model (e.g., \code{lm}, \code{lmerMod}).
#'
#' @return A data frame with columns \code{nobs}, \code{logLik}, \code{AIC}, \code{BIC}, \code{method}.
#'
#' @examples
#' \dontrun{
#' .model_info(lme4::lmer(y ~ x + (1|id), data = df))
#' }
#'
#' @importFrom stats nobs logLik AIC BIC
#' @importFrom lme4 isREML
#' @family print_helpers
#' @export
.model_info <- function(model) {
  data.frame(
    nobs   = tryCatch(nobs(model), error = function(...) NA_integer_),
    logLik = tryCatch(as.numeric(logLik(model)), error = function(...) NA_real_),
    AIC    = tryCatch(AIC(model), error = function(...) NA_real_),
    BIC    = tryCatch(BIC(model), error = function(...) NA_real_),
    method = tryCatch(if (lme4::isREML(model)) "REML" else "ML", error = function(...) NA_character_),
    check.names = FALSE
  )
}


#' Normalize a fixed-effect term string
#'
#' Tidies spacing around \code{":"} and \code{"*"} in interaction terms and collapses
#' multiple spaces. Does not change factor ordering beyond spacing.
#'
#' @param x Character scalar term.
#'
#' @return A normalized character scalar.
#'
#' @examples
#' \dontrun{
#' .norm_fe_term("menses :X")
#' .norm_fe_term("a*b")
#' }
#' @family print_helpers
#' @export
.norm_fe_term <- function(x) {
  x <- gsub("\\s*:\\s*", ":", x)      # menses : X  -> menses:X
  x <- gsub("\\s*\\*\\s*", " * ", x)  # a*b or a *b -> a * b
  x <- gsub("\\s+", " ", x)
  trimws(x)
}


#' Normalize a vector of fixed-effect term strings
#'
#' Applies \code{.norm_fe_term()} to each element and returns unique normalized terms.
#'
#' @param v Character vector of terms.
#'
#' @return A character vector of unique normalized terms.
#'
#' @examples
#' \dontrun{
#' .norm_vec(c("a*b","a * b","a:b"))
#'  }
#' @family print_helpers
#' @export
.norm_vec <- function(v) {
  if (!length(v)) return(character(0))
  unique(vapply(v, .norm_fe_term, character(1)))
}

#' Pretty print with wrapped continuation lines
#'
#' Prints a prefix and a long text with smart wrapping to \code{width}, indenting
#' continuation lines to align after the prefix.
#'
#' @param prefix Character scalar printed at the start of the first line.
#' @param text Character scalar to be wrapped and printed.
#' @param width Target line width (typically \code{getOption("width")}).
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect.
#'
#' @examples
#' \dontrun{
#' .wrapcat("  Info: ", paste(rep("word", 30), collapse = " "), width = 60)
#'  }
#' @family print_helpers
#' @export
.wrapcat <- function(prefix, text, width) {
  if (!nzchar(text)) { cat(prefix, "\n", sep = ""); return(invisible()) }
  indent <- nchar(gsub("[^ ]", " ", prefix))
  wrapped <- strwrap(text, width = max(10, width - indent))
  cat(prefix, wrapped[1], "\n", sep = "")
  if (length(wrapped) > 1L) {
    for (w in wrapped[-1]) cat(strrep(" ", indent), w, "\n", sep = "")
  }
}









