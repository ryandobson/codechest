# ============================================================
# lavaan_syntax_setters.R
#
# The user-facing layer of the lavaan syntax toolkit.
#
# Each set_*() function takes a lavaan model string, walks it block by
# block with a parser (parse_single_block / parse_group_blocks /
# parse_level_blocks), removes the parameter lines it owns, and writes
# fresh ones with equality labels applied according to `mode`. They
# compose: build_model() calls them in sequence to assemble a full
# multi-group or multi-level model.
#
# Layering:
#   lavaan_block_parser.R      splits a model string into blocks
#   lavaan_constraint_engine.R generates/removes individual lines
#   lavaan_syntax_setters.R    (this file) one function per parameter type
#   build_model.R              composes the setters into a whole model
# ============================================================


#' Set factor loadings across blocks
#'
#' Rewrites the `=~` loading line for one factor in every block of a model
#' string, applying cross-block equality labels according to `mode`. This is
#' the workhorse for metric (loading) invariance: `mode = "equal_all"` labels
#' each item's loading identically across groups, `"equal_except"` frees the
#' items named in `free_items`, and `"free_all"` leaves every loading free.
#'
#' @param model_string A lavaan model syntax string.
#' @param parser A block parser: [parse_single_block()], [parse_group_blocks()],
#'   or [parse_level_blocks()].
#' @param factor_name Name of the latent factor whose loadings to set.
#' @param items Character vector of item names loading on `factor_name`.
#' @param mode One of `"equal_all"` (default), `"equal_except"`, or
#'   `"free_all"`.
#' @param free_items Items to leave free when `mode = "equal_except"`.
#' @param scale_id Scale identification: `"none"` (default), `"marker"` (fix a
#'   marker item's loading to 1), or `"effects_coded"` (constrain loadings to
#'   sum to the number of items).
#' @param marker_items Named list, keyed by factor, giving the marker item for
#'   each. Required when `scale_id = "marker"`.
#' @param label_prefix Prefix for generated parameter labels. Default `"l"`.
#'
#' @return The model string with loadings rewritten.
#'
#' @examples
#' ms <- "f1 =~ x1 + x2 + x3"
#' cat(set_loadings(ms, parse_single_block, "f1", c("x1", "x2", "x3")))
#'
#' @family lavaan syntax
#' @seealso [build_model()], which calls this as part of a full model.
#' @export
set_loadings <- function(model_string,
                         parser,
                         factor_name,
                         items,
                         mode = c("equal_all", "equal_except", "free_all"),
                         free_items = NULL,
                         scale_id = c("none", "marker", "effects_coded"),
                         marker_items = NULL,
                         label_prefix = "l") {

  mode     <- match.arg(mode)
  scale_id <- match.arg(scale_id)

  if (mode == "equal_all" && length(free_items)) {
    warning("[set_loadings] free_items was supplied but ignored because ",
            "mode = 'equal_all'.", call. = FALSE)
  }

  if (mode == "equal_except" && (is.null(free_items) || length(free_items) == 0)) {
    warning("[set_loadings] mode = 'equal_except' but free_items was empty. ",
            "No parameters were freed.", call. = FALSE)
  }

  if (scale_id == "effects_coded" && mode != "equal_all") {
    stop("[set_loadings] effects-coded identification requires ",
         "mode = 'equal_all'.", call. = FALSE)
  }

  if (scale_id == "marker") {

    if (is.null(marker_items)) {
      stop("[set_loadings] scale_id = 'marker' but no marker_items supplied.",
           call. = FALSE)
    }

    if (!is.list(marker_items) || is.null(marker_items[[factor_name]])) {
      stop("[set_loadings] Marker items must be supplied as a named list ",
           "with an entry for factor '", factor_name, "'.", call. = FALSE)
    }
  }

  out <- .map_blocks(
    model_string = model_string,
    parser       = parser,
    FUN          = function(block, indices) {

      if (!is.null(indices$type) && indices$type == "prelude") {
        return(block)
      }

      block <- .remove_loadings(block, factor_name)

      rhs <- .generate_loadings(items        = items,
                                mode         = mode,
                                block_id     = indices$block_id %||% NULL,
                                factor_name  = factor_name,
                                scale_id     = scale_id,
                                marker_items = marker_items,
                                free_items   = free_items,
                                label_prefix = label_prefix)

      c(block, paste0("  ", factor_name, " =~ ", rhs))
    }
  )

  # strip old sum constraints for loading labels
  out <- .remove_sum_constraints(out, label_prefix)

  if (scale_id == "effects_coded") {
    ec <- .generate_effects_coded_constraint(label_prefix, length(items),
                                             "n_items")
    out <- .insert_global(model_string = out,
                          parser       = parser,
                          lines        = ec,
                          where        = "prelude")
  }

  out
}


#' Set item intercepts across blocks
#'
#' Rewrites the `~ 1` intercept lines for a factor's items in every block,
#' applying cross-block equality labels according to `mode`. This is the
#' scalar-invariance counterpart to [set_loadings()].
#'
#' @inheritParams set_loadings
#' @param scale_id Scale identification: `"none"` (default), `"marker"`, or
#'   `"effects_coded"` (constrain intercepts to sum to zero).
#' @param label_prefix Prefix for generated parameter labels. Default `"i"`.
#'
#' @return The model string with intercepts rewritten.
#'
#' @examples
#' ms <- "f1 =~ x1 + x2 + x3"
#' cat(set_intercepts(ms, parse_single_block, "f1", c("x1", "x2", "x3")))
#'
#' @family lavaan syntax
#' @export
set_intercepts <- function(model_string,
                           parser,
                           factor_name,
                           items,
                           mode = c("equal_all", "equal_except", "free_all"),
                           free_items = NULL,
                           scale_id = c("none", "marker", "effects_coded"),
                           marker_items = NULL,
                           label_prefix = "i") {

  mode     <- match.arg(mode)
  scale_id <- match.arg(scale_id)

  if (mode == "equal_all" && length(free_items)) {
    warning("[set_intercepts] free_items supplied but ignored because ",
            "mode = 'equal_all'.", call. = FALSE)
  }

  if (mode == "equal_except" &&
      (is.null(free_items) || length(free_items) == 0)) {
    warning("[set_intercepts] mode = 'equal_except' but free_items was empty.",
            call. = FALSE)
  }

  if (scale_id == "effects_coded" && mode != "equal_all") {
    stop("[set_intercepts] effects-coded identification requires ",
         "mode = 'equal_all'.", call. = FALSE)
  }

  if (!is.null(marker_items) && scale_id != "marker") {
    scale_id <- "marker"
    message("'intercepts_marker_items' were provided but ",
            "'intercepts_scale_id' was not set to 'marker'. ",
            "'intercepts_scale_id' was forced to 'marker'. Set ",
            "'intercepts_marker_items' to NULL if another constraint was ",
            "desired.")
  }

  out <- .map_blocks(
    model_string = model_string,
    parser       = parser,
    FUN          = function(block, indices) {

      if (!is.null(indices$type) && indices$type == "prelude") {
        return(block)
      }

      block <- .remove_intercepts(block_lines = block, items = items)

      new <- .generate_intercepts(items        = items,
                                  mode         = mode,
                                  block_id     = indices$block_id %||% NULL,
                                  factor_name  = factor_name,
                                  scale_id     = scale_id,
                                  marker_items = marker_items,
                                  free_items   = free_items,
                                  label_prefix = label_prefix)

      c(block, new)
    }
  )

  out <- .remove_sum_constraints(out, label_prefix)

  if (scale_id == "effects_coded") {
    ec <- .generate_effects_coded_constraint(label_prefix = label_prefix,
                                             n_items      = length(items),
                                             sum_target   = "zero")
    out <- .insert_global(model_string = out,
                          parser       = parser,
                          lines        = ec,
                          where        = "prelude")
  }

  out
}


#' Set item residual variances across blocks
#'
#' Rewrites the `~~` residual variance lines for a set of items in every
#' block. Constraining these across groups is what distinguishes strict
#' invariance from scalar invariance.
#'
#' @inheritParams set_loadings
#' @param label_prefix Prefix for generated parameter labels. Default `"r"`.
#'
#' @return The model string with residual variances rewritten.
#'
#' @examples
#' ms <- "f1 =~ x1 + x2 + x3"
#' cat(set_residuals(ms, parse_single_block, c("x1", "x2", "x3")))
#'
#' @family lavaan syntax
#' @export
set_residuals <- function(model_string,
                          parser,
                          items,
                          mode = c("equal_all", "equal_except", "free_all"),
                          free_items = NULL,
                          label_prefix = "r") {

  mode <- match.arg(mode)

  if (mode == "equal_except" && (is.null(free_items) || length(free_items) == 0)) {
    warning("[set_residuals] mode = 'equal_except' but free_items was empty. ",
            "No parameters were freed.", call. = FALSE)
  }

  .map_blocks(
    model_string = model_string,
    parser       = parser,
    FUN          = function(block, indices) {

      if (!is.null(indices$type) && indices$type == "prelude") {
        return(block)
      }

      block <- .remove_residuals(block_lines  = block,
                                 items        = items,
                                 label_prefix = label_prefix)

      new <- .generate_residuals(items        = items,
                                 mode         = mode,
                                 block_id     = indices$block_id %||% NULL,
                                 free_items   = free_items,
                                 label_prefix = label_prefix)

      c(block, new)
    }
  )
}


#' Set residual covariances between items
#'
#' Adds `~~` covariance lines between named item pairs, with cross-block
#' equality labels according to `mode`. Use this for correlated residuals
#' between items that share method variance or wording.
#'
#' @inheritParams set_loadings
#' @param items Character vector of all items in scope. Any pre-existing
#'   covariance among these is removed before the new lines are written.
#' @param item_covars A named character vector of item pairs, where names are
#'   the left-hand items and values the right-hand items (e.g.
#'   `c(x1 = "x2")` for a covariance between `x1` and `x2`).
#' @param free_pairs Pairs to leave free when `mode = "equal_except"`.
#' @param label_prefix Prefix for generated parameter labels. Default `"c"`.
#'
#' @return The model string with item covariances rewritten.
#'
#' @examples
#' ms <- "f1 =~ x1 + x2 + x3"
#' cat(set_item_covariances(ms, parse_single_block,
#'                          items = c("x1", "x2", "x3"),
#'                          item_covars = c(x1 = "x2")))
#'
#' @family lavaan syntax
#' @export
set_item_covariances <- function(model_string,
                                 parser,
                                 items,
                                 item_covars,
                                 mode = c("equal_all", "equal_except", "free_all"),
                                 free_pairs = NULL,
                                 label_prefix = "c") {

  # Pairs are carried as names -> values, so an unnamed vector has no left
  # side at all. Caught here because the failure would otherwise surface
  # from data.frame() as "differing number of rows: 0, 2".
  if (length(item_covars) && is.null(names(item_covars))) {
    stop("[set_item_covariances] `item_covars` must be a named vector of ",
         "item pairs, e.g. c(x1 = \"x2\") to covary x1 with x2.",
         call. = FALSE)
  }

  covar_items <- unique(c(names(item_covars), as.character(item_covars)))

  missing_items <- setdiff(covar_items, items)
  if (length(missing_items)) {
    stop("[set_item_covariances] The following items in `item_covars` were ",
         "not found in `items`: ", paste(missing_items, collapse = ", "),
         call. = FALSE)
  }

  mode <- match.arg(mode)

  item_covars <- data.frame(lhs = names(item_covars),
                            rhs = as.character(item_covars),
                            stringsAsFactors = FALSE)

  if (mode == "equal_all" && length(free_pairs)) {
    warning("[set_item_covariances] free_pairs supplied but ignored because ",
            "mode = 'equal_all'.", call. = FALSE)
  }
  if (mode == "equal_except" && (is.null(free_pairs) || length(free_pairs) == 0)) {
    warning("[set_item_covariances] mode = 'equal_except' but free_pairs was ",
            "empty. No covariance was freed.", call. = FALSE)
  }

  .map_blocks(
    model_string = model_string,
    parser       = parser,
    FUN          = function(block, indices) {

      if (!is.null(indices$type) && indices$type == "prelude") {
        return(block)
      }

      block <- .remove_covariances(block, vars = items)

      new <- .generate_covariances(covars       = item_covars,
                                   mode         = mode,
                                   block_id     = indices$block_id %||% NULL,
                                   free_pairs   = free_pairs,
                                   label_prefix = label_prefix)

      c(block, new)
    }
  )
}


#' Set covariances between latent factors
#'
#' Writes `~~` lines for every pair of the named factors. Unlike
#' [set_item_covariances()], the pairs are generated automatically from all
#' combinations of `factors`.
#'
#' @inheritParams set_loadings
#' @param factors Character vector of latent factor names.
#' @param mode One of `"free_all"` (default), `"equal_all"`,
#'   `"equal_except"`, or `"fixed_zero"` (orthogonal factors).
#' @param free_pairs Pairs to leave free when `mode = "equal_except"`.
#' @param label_prefix Prefix for generated parameter labels. Default `"fc"`.
#'
#' @return The model string with factor covariances rewritten.
#'
#' @examples
#' ms <- "f1 =~ x1 + x2 + x3\nf2 =~ y1 + y2 + y3"
#' cat(set_factor_covariances(ms, parse_single_block, c("f1", "f2")))
#'
#' @family lavaan syntax
#' @export
set_factor_covariances <- function(model_string,
                                   parser,
                                   factors,
                                   mode = c("free_all", "equal_all",
                                            "equal_except", "fixed_zero"),
                                   free_pairs = NULL,
                                   label_prefix = "fc") {

  mode <- match.arg(mode)

  covars <- .make_factor_pairs(factors)

  model_factors <- .extract_factor_names(model_string)

  missing_factors <- setdiff(factors, model_factors)
  if (length(missing_factors)) {
    stop("[set_factor_covariances] The following factors were not found in ",
         "`model_string`: ", paste(missing_factors, collapse = ", "),
         call. = FALSE)
  }

  if (mode == "equal_all" && length(free_pairs)) {
    warning("[set_factor_covariances] free_pairs ignored because ",
            "mode = 'equal_all'.", call. = FALSE)
  }

  if (mode == "equal_except" &&
      (is.null(free_pairs) || length(free_pairs) == 0)) {
    warning("[set_factor_covariances] mode = 'equal_except' but free_pairs ",
            "was empty.", call. = FALSE)
  }

  .map_blocks(
    model_string = model_string,
    parser       = parser,
    FUN          = function(block, indices) {

      if (!is.null(indices$type) && indices$type == "prelude") {
        return(block)
      }

      block <- .remove_covariances(block, vars = factors)

      new <- .generate_covariances(covars       = covars,
                                   mode         = mode,
                                   block_id     = indices$block_id %||% NULL,
                                   free_pairs   = free_pairs,
                                   label_prefix = label_prefix)

      c(block, new)
    }
  )
}


#' Set a factor's variance across blocks
#'
#' Writes the `factor ~~ factor` variance line in every block. Fixing the
#' variance to 1 in one or all blocks is one of the two standard ways to give
#' a latent variable a scale (the other being a marker item, see
#' [set_loadings()]).
#'
#' @details
#' This function carries a known limitation inherited from the original
#' implementation, and emits a warning on every call to say so:
#' `.remove_factor_variances()` matches on `factor ~~` and so will also strip
#' *covariance* lines involving that factor, not just its variance. If you
#' are setting both factor variances and factor covariances, set the
#' variances first. The warning is deliberate — it documents real current
#' behavior rather than a hypothetical risk, and fixing the underlying regex
#' needs a battery of invariance models to validate against before it can be
#' trusted.
#'
#' @inheritParams set_loadings
#' @param variances_mode One of `"first_one"` (fix the first block's variance
#'   to 1), `"all_one"`, `"groups_one"` (fix only the blocks named in
#'   `groups_to_fix`), `"none"`, or `"all_equal"`.
#' @param groups_to_fix Character vector of group names to fix, used when
#'   `variances_mode = "groups_one"`.
#' @param label_prefix Prefix for generated parameter labels. Default `"v"`.
#'
#' @return The model string with factor variance lines rewritten.
#'
#' @examples
#' ms <- "f1 =~ x1 + x2 + x3"
#' cat(suppressWarnings(set_variances(ms, parse_single_block, "f1")))
#'
#' @family lavaan syntax
#' @export
set_variances <- function(model_string,
                          parser,
                          factor_name,
                          variances_mode = c("first_one", "all_one",
                                             "groups_one", "none", "all_equal"),
                          groups_to_fix = NULL,
                          label_prefix = "v") {

  warning("As of 1/23/26, there is an error in the removal of factor ",
          "variances. If factor covariances are generated, it will also ",
          "remove factor covariances for that factor. An additional check ",
          "that it is factor ~~ factor needs to be done.", call. = FALSE)

  variances_mode <- match.arg(variances_mode)

  if (variances_mode == "groups_one" && is.null(groups_to_fix)) {
    warning("'variances_mode' was specified as 'groups_one' but no ",
            "'groups_to_fix' were supplied. The first group was fixed to 1 ",
            "to identify the model.", call. = FALSE)
    variances_mode <- "first_one"
  }

  .map_blocks(
    model_string = model_string,
    parser       = parser,
    FUN          = function(block, indices) {

      if (!is.null(indices$type) && indices$type == "prelude") {
        return(block)
      }

      block <- .remove_factor_variances(block, factor_name = factor_name)

      line <- .generate_factor_variance(factor_name   = factor_name,
                                        mode          = variances_mode,
                                        block_id      = indices$block_id,
                                        group_name    = indices$group_name,
                                        groups_to_fix = groups_to_fix,
                                        label_prefix  = label_prefix)

      c(block, line)
    }
  )
}


#' Set a factor's latent mean across blocks
#'
#' Writes the `factor ~ ...*1` latent mean line in every block. Fixing the
#' mean to zero in a reference group is the standard way to identify latent
#' means in a multi-group model.
#'
#' @inheritParams set_loadings
#' @param means_mode One of `"first_zero"` (fix the first block's mean to 0),
#'   `"all_zero"`, `"groups_zero"` (fix only the blocks named in
#'   `groups_to_fix`), `"none"`, or `"all_equal"`.
#' @param groups_to_fix Character vector of group names to fix, used when
#'   `means_mode = "groups_zero"`.
#' @param label_prefix Prefix for generated parameter labels. Default `"m"`.
#'
#' @return The model string with factor mean lines rewritten.
#'
#' @examples
#' ms <- "f1 =~ x1 + x2 + x3"
#' cat(set_means(ms, parse_single_block, "f1"))
#'
#' @family lavaan syntax
#' @export
set_means <- function(model_string,
                      parser,
                      factor_name,
                      means_mode = c("first_zero", "all_zero",
                                     "groups_zero", "none", "all_equal"),
                      groups_to_fix = NULL,
                      label_prefix = "m") {

  means_mode <- match.arg(means_mode)

  if (means_mode == "groups_zero" && is.null(groups_to_fix)) {
    warning("'means_mode' was specified as 'groups_zero' but no ",
            "'groups_to_fix' were supplied. The first group was fixed to 0 ",
            "to identify the model.", call. = FALSE)
    means_mode <- "first_zero"
  }

  .map_blocks(
    model_string = model_string,
    parser       = parser,
    FUN          = function(block, indices) {

      if (!is.null(indices$type) && indices$type == "prelude") {
        return(block)
      }

      block <- .remove_factor_means(block, factor_name)

      line <- .generate_factor_mean(factor_name   = factor_name,
                                    mode          = means_mode,
                                    block_id      = indices$block_id,
                                    group_name    = indices$group_name,
                                    groups_to_fix = groups_to_fix,
                                    label_prefix  = label_prefix)

      c(block, line)
    }
  )
}


#' Normalize a per-factor option argument
#'
#' Lets build_model() accept one option for every factor (a scalar), or a
#' factor-keyed named list/vector for per-factor control, and always hand the
#' rest of the pipeline the same shape: a list keyed by factor name.
#'
#' @param x NULL, a scalar, a named character vector, or a named list.
#' @param factor_names Character vector of factor names to key by.
#' @param arg_name Name of the calling argument, used in error messages.
#' @return A list keyed by `factor_names`.
#' @noRd
.normalize_factor_arg <- function(x, factor_names, arg_name) {

  # An unnamed multi-element character vector is the formal default of a
  # match.arg() argument that was never actually set, so collapse it to the
  # first option rather than treating it as a per-factor specification.
  if (is.character(x) && length(x) > 1L && is.null(names(x))) {
    x <- x[1L]
  }

  if (is.null(x)) {
    out <- vector("list", length(factor_names))
    names(out) <- factor_names
    return(out)
  }

  if (is.character(x) && length(x) == 1L) {
    out <- rep(list(x), length(factor_names))
    names(out) <- factor_names
    return(out)
  }

  if (is.character(x) && length(x) > 1L) {

    if (is.null(names(x))) {
      stop("[build_model] ", arg_name,
           " must be named when supplied as a character vector.",
           call. = FALSE)
    }

    missing <- setdiff(factor_names, names(x))
    if (length(missing)) {
      stop("[build_model] ", arg_name, " missing entries for factors: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }

    return(as.list(x[factor_names]))
  }

  if (is.list(x)) {

    if (is.null(names(x))) {
      stop("[build_model] ", arg_name, " must be a named list keyed by factor.",
           call. = FALSE)
    }

    missing <- setdiff(factor_names, names(x))
    if (length(missing)) {
      stop("[build_model] ", arg_name, " missing entries for factors: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }

    return(x[factor_names])
  }

  stop("[build_model] Invalid specification for ", arg_name,
       ". Must be NULL, scalar, or factor-keyed.", call. = FALSE)
}


#' Normalize a per-factor groups_to_fix argument
#'
#' Same idea as `.normalize_factor_arg()`, but for arguments whose per-factor
#' value is itself a character vector of group names, so a bare character
#' vector means "these groups, for every factor" rather than "one value per
#' factor".
#'
#' @param x NULL, a character vector of group names, or a factor-keyed list.
#' @param factor_names Character vector of factor names to key by.
#' @param arg_name Name of the calling argument, used in error messages.
#' @return A list keyed by `factor_names`, each element NULL or character.
#' @noRd
.normalize_factor_groups_to_fix <- function(x, factor_names, arg_name) {

  if (is.null(x)) {
    out <- vector("list", length(factor_names))
    names(out) <- factor_names
    return(out)
  }

  if (is.character(x)) {
    out <- rep(list(x), length(factor_names))
    names(out) <- factor_names
    return(out)
  }

  if (is.list(x)) {

    if (is.null(names(x))) {
      stop("[build_model] ", arg_name,
           " must be a named list keyed by factor when supplied as a list.",
           call. = FALSE)
    }

    missing <- setdiff(factor_names, names(x))
    if (length(missing)) {
      stop("[build_model] ", arg_name, " missing entries for factors: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }

    for (fac in factor_names) {
      if (!is.null(x[[fac]]) && !is.character(x[[fac]])) {
        stop("[build_model] ", arg_name, " for factor '", fac,
             "' must be NULL or a character vector of group names.",
             call. = FALSE)
      }
    }

    return(x[factor_names])
  }

  stop("[build_model] Invalid specification for ", arg_name,
       ". Must be NULL, character vector, or named list.", call. = FALSE)
}
