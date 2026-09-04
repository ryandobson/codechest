# ============================================================
# var_labels.R
#
# Human-readable variable labels, in base R.
#
# A label is just an attribute:
#
#   attr(df$weight, "label") <- "Dried plant weight (g)"
#
# That is plain base R. No package is needed to set one, read one, or
# have it survive saveRDS(). apply_variable_info() reaches the same
# result through haven::labelled(), which additionally attaches value
# labels and a vctrs class; use that when the value labels matter, and
# these when they do not.
#
# WHAT PRESERVES A LABEL
#   kept:    dplyr verbs (filter, mutate, arrange, select), as_tibble(),
#            rbind(), model.frame(), arithmetic on the column, saveRDS()
#   dropped: base R row subsetting, df[i, ] and subset(), and c()
#
# The base R subsetting case is the one to watch, because it is the one
# people expect to be safe. Label after subsetting, or use dplyr.
# ============================================================


#' Set human-readable variable labels
#'
#' Attaches a `"label"` attribute to one or more columns. Functions that plot
#' or tabulate can then use the label instead of the bare column name, so
#' "Dried plant weight (g)" appears on an axis without being typed at every
#' call site.
#'
#' This is base R: the label is an ordinary attribute. [apply_variable_info()]
#' does the same job through `haven::labelled()`, which also carries value
#' labels for coded variables; prefer that when you need those, and this when
#' you only want a display name.
#'
#' @section What preserves a label:
#' Labels survive dplyr verbs, `as_tibble()`, `rbind()`, `model.frame()`,
#' arithmetic on the column, and `saveRDS()`.
#'
#' They are **dropped** by base R row subsetting (`df[i, ]` and `subset()`) and
#' by `c()`. The subsetting case is worth knowing, since it is the one that
#' looks safe. Either label after subsetting, or subset with dplyr.
#'
#' A CSV cannot carry labels at all, so a write/read round trip loses them.
#'
#' @param df A data frame.
#' @param ... Labels as `column = "Label"` pairs.
#' @param labels Alternatively, a named character vector or list, which is
#'   easier to build programmatically. Merged with `...` if both are given.
#' @param warn_missing Warn about names that are not columns of `df`.
#'   Default `TRUE`.
#'
#' @return `df`, with labels attached.
#'
#' @examples
#' d <- set_var_labels(PlantGrowth,
#'                     weight = "Dried plant weight (g)",
#'                     group  = "Treatment condition")
#' var_labels(d)
#'
#' # or from a vector, which is easier to keep in one place
#' lab <- c(weight = "Dried plant weight (g)", group = "Treatment condition")
#' d2 <- set_var_labels(PlantGrowth, labels = lab)
#' identical(var_labels(d), var_labels(d2))
#'
#' @seealso [var_labels()] to read them back, [apply_variable_info()] for the
#'   haven route with value labels.
#' @family labelling
#' @export
set_var_labels <- function(df, ..., labels = NULL, warn_missing = TRUE) {

  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  dots <- list(...)
  if (length(dots) && is.null(names(dots))) {
    stop("Labels in `...` must be named, e.g. weight = \"Dried weight (g)\".",
         call. = FALSE)
  }
  if (!is.null(labels)) {
    if (is.null(names(labels)) || any(!nzchar(names(labels)))) {
      stop("`labels` must be a named vector or list.", call. = FALSE)
    }
    labels <- as.list(labels)
    # ... wins on a clash, being the more explicit of the two
    dots <- utils::modifyList(labels, dots)
  }
  if (!length(dots)) return(df)

  bad <- vapply(dots, function(x) !is.character(x) || length(x) != 1L,
                logical(1))
  if (any(bad)) {
    stop("Each label must be a single string. Problem with: ",
         paste(names(dots)[bad], collapse = ", "), call. = FALSE)
  }

  unknown <- setdiff(names(dots), names(df))
  if (length(unknown) && warn_missing) {
    warning("Not columns of `df`, so ignored: ",
            paste(unknown, collapse = ", "), call. = FALSE)
  }

  for (v in intersect(names(dots), names(df))) {
    attr(df[[v]], "label") <- dots[[v]]
  }
  df
}

#' Read the variable labels off a data frame
#'
#' @param df A data frame.
#' @param missing What to return for a column with no label: `"name"`
#'   (default) gives the column name, which is what a plot would fall back to
#'   anyway; `"na"` gives `NA_character_`, which is what you want when checking
#'   which columns still need labelling.
#'
#' @return A named character vector, one element per column.
#'
#' @examples
#' d <- set_var_labels(PlantGrowth, weight = "Dried plant weight (g)")
#' var_labels(d)
#'
#' # which columns still need a label?
#' names(which(is.na(var_labels(d, missing = "na"))))
#'
#' @seealso [set_var_labels()]
#' @family labelling
#' @export
var_labels <- function(df, missing = c("name", "na")) {

  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)
  missing <- match.arg(missing)

  out <- vapply(names(df), function(v) {
    lab <- attr(df[[v]], "label", exact = TRUE)
    if (is.null(lab) || !is.character(lab) || length(lab) != 1L || !nzchar(lab)) {
      if (missing == "name") v else NA_character_
    } else {
      lab
    }
  }, character(1))

  stats::setNames(out, names(df))
}
