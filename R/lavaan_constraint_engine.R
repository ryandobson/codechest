# Constraint-generation engine for the lavaan-syntax-building toolkit -------
#
# Where lavaan_block_parser.R handles *where* a set_*() function edits (which
# block of the model string), this file handles *what* it writes into that
# block: the actual lavaan parameter lines (`l1*x1`, `x1 ~ i1*1`, ...) and
# the logic that decides, item by item, whether a parameter should be fixed,
# freely estimated, or given a shared label.
#
# Two small "label engines" hold that decision logic:
#   - .parameter_label_engine() decides item-level labels (loadings,
#     intercepts, residuals, item covariances) given a mode
#     ("equal_all"/"equal_except"/"free_all"/"fixed_zero") and optional
#     marker-item / free-item overrides.
#   - .parameter_engine_mv() decides factor-level labels (latent means and
#     variances) given a mode ("first_zero"/"all_zero"/"groups_zero"/
#     "none"/"all_equal", and the "_one" variants for variances).
#
# The `.generate_*()` functions turn those decisions into actual lavaan
# syntax lines, and the `.remove_*()` functions strip out the *previous*
# version of those lines so a set_*() call can be re-run without leaving
# stale parameters behind.


#' Decide an item-level parameter's label
#'
#' The label engine behind loadings, intercepts, residuals, and item
#' covariances: given one item and a `mode`, decides whether it should be
#' a scale-identification marker, fixed to zero, given a shared ("fixed")
#' label, or freely estimated (optionally with an explicit per-block label
#' so it stays distinguishable across groups).
#'
#' @param item The item (or, for covariances, `"lhs_rhs"` pair id) being
#'   labelled.
#' @param item_index The item's 1-based position within its factor/vector;
#'   used to build the default label (`paste0(label_prefix, item_index)`).
#' @param block_id The block's id (e.g. group number), or `NULL` outside a
#'   multi-group/multilevel context. Only used when a parameter is freed
#'   and `explicit_labels_when_free = TRUE`.
#' @param mode One of `"equal_all"`, `"equal_except"`, `"free_all"`, or
#'   `"fixed_zero"`.
#' @param factor_name The factor the item belongs to; required to check
#'   `marker_items`.
#' @param scale_id `"none"`, `"marker"`, or `"effects_coded"`.
#' @param marker_items A named list keyed by factor name, giving the marker
#'   item(s) for that factor when `scale_id = "marker"`.
#' @param free_items Items to free under `mode = "equal_except"`.
#' @param label_prefix The label prefix (e.g. `"l"`, `"i"`, `"r"`).
#' @param explicit_labels_when_free If `TRUE` (default), a freed parameter
#'   gets an explicit `<label>_g<block_id>` label rather than `NA`, so it
#'   stays uniquely identified per block.
#'
#' @return A list with `type` (one of `"marker"`, `"fixed_zero"`, `"fixed"`,
#'   `"group_free"`, `"free_na"`) and `label` (the label string, or `NULL`/
#'   `NA` when not applicable).
#' @keywords internal
.parameter_label_engine <- function(
    item,
    item_index,
    block_id = NULL,
    mode,
    factor_name = NULL,
    scale_id = c("none", "marker", "effects_coded"),
    marker_items = NULL,
    free_items = NULL,
    label_prefix = "p",
    explicit_labels_when_free = TRUE
) {
  scale_id <- match.arg(scale_id)
  lab_base <- paste0(label_prefix, item_index)

  # ---- Marker rule --------------------------------------------------------
  if (scale_id == "marker" &&
      !is.null(marker_items) &&
      !is.null(factor_name) &&
      !is.null(marker_items[[factor_name]]) &&
      item %in% marker_items[[factor_name]]) {

    return(list(type = "marker", label = NULL))
  }

  # ---- fixed_zero ---------------------------------------------------------
  if (mode == "fixed_zero") {
    return(list(type = "fixed_zero", label = NULL))
  }

  # ---- equal_all ------------------------------------------------------------
  if (mode == "equal_all") {
    return(list(type = "fixed", label = lab_base))
  }

  # ---- free_all -------------------------------------------------------------
  if (mode == "free_all") {
    if (explicit_labels_when_free && !is.null(block_id)) {
      return(list(type = "group_free", label = paste0(lab_base, "_g", block_id)))
    }
    return(list(type = "free_na", label = NA_character_))
  }

  # ---- equal_except (freed case) --------------------------------------------
  if (mode == "equal_except" &&
      !is.null(free_items) &&
      item %in% free_items) {

    if (explicit_labels_when_free && !is.null(block_id)) {
      return(list(type = "group_free", label = paste0(lab_base, "_g", block_id)))
    }
    return(list(type = "free_na", label = NA_character_))
  }

  # ---- equal_except (constrained case) --------------------------------------
  list(type = "fixed", label = lab_base)
}


#' Decide a factor-level parameter's label (means and variances)
#'
#' The label engine behind latent means and variances: given a `mode`,
#' decides whether the factor's mean/variance should be fixed to an
#' identifying constant (0 for means, 1 for variances), given a shared
#' label across all blocks, or freely estimated per block.
#'
#' @param mode One of `"all_equal"`, `"first_zero"`/`"first_one"`,
#'   `"groups_zero"`/`"groups_one"`, `"all_zero"`/`"all_one"`, or `"none"`.
#'   The `_zero` variants are for means, `_one` for variances.
#' @param block_id The current block's (group's) 1-based id.
#' @param group_name The current block's group name, used to check
#'   `groups_to_fix` when it's supplied as group names rather than ids.
#' @param groups_to_fix Optional group ids or names to fix under
#'   `mode = "groups_zero"`/`"groups_one"`.
#' @param label_prefix The label prefix (e.g. `"m"`, `"v"`).
#' @param equal_label The label suffix used under `mode = "all_equal"`
#'   (default `"1"`, giving e.g. `"m1"`).
#'
#' @return A list with `fixed` (logical), `fixed_value` (0 or 1, when
#'   `fixed`), `label` (a label string, or `NULL`), and `free` (logical).
#' @keywords internal
.parameter_engine_mv <- function(mode,
                                  block_id,
                                  group_name,
                                  groups_to_fix = NULL,
                                  label_prefix,
                                  equal_label = "1") {

  spec <- list(fixed = FALSE, fixed_value = NULL, label = NULL, free = TRUE)

  is_group_match <- function() {
    if (is.null(groups_to_fix)) return(FALSE)
    if (is.numeric(groups_to_fix)) return(block_id %in% groups_to_fix)
    trimws(group_name) %in% trimws(as.character(groups_to_fix))
  }

  if (mode == "all_equal") {
    spec$label <- paste0(label_prefix, equal_label)
    return(spec)
  }

  if (mode %in% c("first_zero", "first_one") && block_id == 1L) {
    spec$fixed <- TRUE
    spec$fixed_value <- ifelse(grepl("zero", mode), 0, 1)
    spec$free <- FALSE
    return(spec)
  }

  if (mode %in% c("groups_zero", "groups_one") && is_group_match()) {
    spec$fixed <- TRUE
    spec$fixed_value <- ifelse(grepl("zero", mode), 0, 1)
    spec$free <- FALSE
    return(spec)
  }

  if (mode %in% c("all_zero", "all_one")) {
    spec$fixed <- TRUE
    spec$fixed_value <- ifelse(grepl("zero", mode), 0, 1)
    spec$free <- FALSE
    return(spec)
  }

  if (mode == "none") {
    spec$label <- paste0(label_prefix, block_id)
    return(spec)
  }

  spec
}


#' Remove existing loading lines for a factor
#' @param block_lines A block's character vector of lavaan syntax lines.
#' @param latent The factor name whose `<latent> =~ ...` line should be dropped.
#' @return `block_lines` with that factor's loading line (if any) removed.
#' @keywords internal
.remove_loadings <- function(block_lines, latent) {
  keep <- !grepl(paste0("^\\s*", latent, "\\s*=~"), block_lines)
  block_lines[keep]
}


#' Remove existing intercept lines for a set of items
#'
#' Removes any `item ~ <label>*1` line (regardless of label prefix) for the
#' given `items`, so a fresh set of intercepts can be written without
#' leaving the old ones behind. Does not remove loading lines (`item ~
#' x1 + x2`), only intercept lines (which always end in `*1`).
#'
#' @param block_lines A block's character vector of lavaan syntax lines.
#' @param items The items whose intercept line(s) should be dropped.
#' @param label_prefix Unused; kept for call-signature symmetry with the
#'   other `.remove_*()` functions.
#' @return `block_lines` with matching intercept lines removed.
#' @keywords internal
.remove_intercepts <- function(block_lines, items, label_prefix = NULL) {
  item_pattern <- paste(items, collapse = "|")

  pat <- paste0(
    "^\\s*(", item_pattern, ")\\s*~\\s*(?:[^*]+|NA|0)\\*1\\s*$"
  )

  block_lines[!grepl(pat, block_lines, perl = TRUE)]
}


#' Remove existing residual-variance lines for a set of items
#' @param block_lines A block's character vector of lavaan syntax lines.
#' @param items The items whose residual-variance line(s) should be dropped.
#' @param label_prefix The residual label prefix (e.g. `"r"`) used to match
#'   previously generated labels.
#' @return `block_lines` with matching residual lines removed.
#' @keywords internal
.remove_residuals <- function(block_lines, items, label_prefix) {
  item_pattern <- paste(items, collapse = "|")

  pat <- paste0(
    "^\\s*(", item_pattern, ")\\s*~~\\s*(",
    label_prefix, "[0-9]+(_g[0-9]+)?",
    "|NA",
    ")\\*",
    "(", item_pattern, ")\\s*$"
  )

  block_lines[!grepl(pat, block_lines)]
}


#' Remove existing covariance lines between a set of variables
#'
#' Used for both item covariances and factor covariances: removes any
#' `lhs ~~ [label*]rhs` line where `lhs`/`rhs` is any pairing drawn from
#' `vars`.
#'
#' @param lines A block's character vector of lavaan syntax lines.
#' @param vars The variables (items or factor names) whose pairwise
#'   covariance lines should be dropped.
#' @return `lines` with matching covariance lines removed.
#' @keywords internal
.remove_covariances <- function(lines, vars) {
  if (length(vars) < 2L) return(lines)

  universe <- .make_factor_pairs(vars)

  keep <- logical(length(lines))

  parse_covariance <- function(line) {
    if (!grepl("~~", line)) return(NULL)

    parts <- strsplit(line, "~~", fixed = TRUE)[[1]]

    lhs <- trimws(parts[1])

    rhs <- trimws(parts[2])
    rhs <- sub("^([^*]+\\*)", "", rhs)  # drop label*
    rhs <- trimws(rhs)

    c(lhs, rhs)
  }

  for (i in seq_along(lines)) {

    pair <- parse_covariance(lines[i])

    if (is.null(pair)) {
      keep[i] <- TRUE
      next
    }

    match <- (
      (pair[1] == universe$lhs & pair[2] == universe$rhs) |
        (pair[1] == universe$rhs & pair[2] == universe$lhs)
    )

    keep[i] <- !any(match)
  }

  lines[keep]
}


#' Remove existing latent-variance line for a factor
#' @param block_lines A block's character vector of lavaan syntax lines.
#' @param factor_name The factor whose `<factor> ~~ ...*<factor>` line
#'   should be dropped.
#' @return `block_lines` with that factor's variance line (if any) removed.
#' @keywords internal
.remove_factor_variances <- function(block_lines, factor_name) {
  block_lines[!grepl(paste0("^\\s*", factor_name, "\\s*~~\\s*"), block_lines)]
}


#' Remove existing latent-mean line for a factor
#' @param block_lines A block's character vector of lavaan syntax lines.
#' @param factor_name The factor whose `<factor> ~ ...*1` line should be
#'   dropped. Does not remove loading lines (`<factor> =~ x1 + x2`).
#' @return `block_lines` with that factor's mean line (if any) removed.
#' @keywords internal
.remove_factor_means <- function(block_lines, factor_name) {
  pat <- paste0("^\\s*", factor_name, "\\s*~\\s*([^*]+)\\*1\\s*$")
  block_lines[!grepl(pat, block_lines)]
}


#' Remove existing effects-coding sum constraints for a label prefix
#'
#' Strips lines like `l1 + l2 + l3 == 3`, so an effects-coded identification
#' constraint can be regenerated without leaving a stale copy behind.
#'
#' @param model_string A full lavaan model-syntax string.
#' @param label_prefix The label prefix (e.g. `"l"`, `"i"`) whose sum
#'   constraint line should be dropped.
#' @return `model_string` with the matching sum-constraint line removed.
#' @keywords internal
.remove_sum_constraints <- function(model_string, label_prefix) {

  lines <- strsplit(model_string, "\n", fixed = TRUE)[[1]]

  pat <- paste0(
    "^\\s*", label_prefix, "[0-9]+",
    "(\\s*\\+\\s*", label_prefix, "[0-9]+)*",
    "\\s*==\\s*[0-9]+\\s*$"
  )

  lines <- lines[!grepl(pat, lines)]

  paste(lines, collapse = "\n")
}


#' Generate a factor's loading line right-hand side
#'
#' Builds the `<label>*item + <label>*item + ...` right-hand side for a
#' `<factor> =~ ...` line, deciding each item's label via
#' [.parameter_label_engine()].
#'
#' @inheritParams .parameter_label_engine
#' @param items The factor's items, in order.
#' @return A single string: the loading line's right-hand side.
#' @keywords internal
.generate_loadings <- function(items,
                                mode,
                                block_id = NULL,
                                factor_name,
                                scale_id,
                                marker_items,
                                free_items,
                                label_prefix,
                                explicit_labels_when_free = TRUE) {
  out <- character(length(items))

  for (k in seq_along(items)) {

    info <- .parameter_label_engine(
      items[k], k,
      block_id     = block_id,
      mode         = mode,
      factor_name  = factor_name,
      scale_id     = scale_id,
      marker_items = marker_items,
      free_items   = free_items,
      label_prefix = label_prefix,
      explicit_labels_when_free = explicit_labels_when_free
    )

    if (info$type == "marker") {
      out[k] <- paste0("1*", items[k])
    } else {
      out[k] <- paste0(info$label, "*", items[k])
    }
  }

  paste(out, collapse = " + ")
}


#' Generate intercept lines for a set of items
#' @inheritParams .parameter_label_engine
#' @param items The items to generate intercept lines for.
#' @return A character vector of `item ~ <label>*1` lines, one per item.
#' @keywords internal
.generate_intercepts <- function(items,
                                  mode,
                                  block_id = NULL,
                                  factor_name = NULL,
                                  scale_id,
                                  marker_items,
                                  free_items,
                                  label_prefix,
                                  explicit_labels_when_free = TRUE) {

  out <- character(length(items))

  for (k in seq_along(items)) {
    item <- items[k]

    info <- .parameter_label_engine(
      item          = item,
      item_index    = k,
      block_id      = block_id,
      mode          = mode,
      factor_name   = factor_name,
      scale_id      = scale_id,
      marker_items  = marker_items,
      free_items    = free_items,
      label_prefix  = label_prefix,
      explicit_labels_when_free = explicit_labels_when_free
    )

    if (info$type == "marker") {
      out[k] <- paste0("  ", item, " ~ 0*1")
    } else {
      out[k] <- paste0("  ", item, " ~ ", info$label, "*1")
    }
  }

  out
}


#' Generate residual-variance lines for a set of items
#' @param items The items to generate residual-variance lines for.
#' @param mode One of `"equal_all"`, `"equal_except"`, `"free_all"`.
#' @param block_id The current block's id, or `NULL`.
#' @param free_items Items to free under `mode = "equal_except"`.
#' @param label_prefix The residual label prefix (e.g. `"r"`).
#' @param explicit_labels_when_free See [.parameter_label_engine()].
#' @return A character vector of `item ~~ <label>*item` lines, one per item.
#' @keywords internal
.generate_residuals <- function(items,
                                 mode,
                                 block_id = NULL,
                                 free_items,
                                 label_prefix,
                                 explicit_labels_when_free = TRUE) {
  out <- character(length(items))

  for (k in seq_along(items)) {
    item <- items[k]

    info <- .parameter_label_engine(
      item        = item,
      item_index  = k,
      block_id    = block_id,
      mode        = mode,
      scale_id    = "none",
      free_items    = free_items,
      label_prefix  = label_prefix,
      explicit_labels_when_free = explicit_labels_when_free
    )

    if (info$type == "free_na") {
      out[k] <- paste0("  ", item, " ~~ NA*", item)
    } else {
      out[k] <- paste0("  ", item, " ~~ ", info$label, "*", item)
    }
  }

  out
}


#' Generate covariance lines for a set of variable pairs
#'
#' Shared by [set_item_covariances()] and [set_factor_covariances()]: given
#' a data frame of `lhs`/`rhs` pairs, generates one `lhs ~~ [label*]rhs`
#' line per pair.
#'
#' @param covars A data frame with `lhs` and `rhs` character columns (one
#'   row per pair), as produced by [.make_factor_pairs()] or supplied
#'   directly for item covariances.
#' @param mode One of `"free_all"`, `"equal_all"`, `"equal_except"`,
#'   `"fixed_zero"`.
#' @param block_id The current block's id, or `NULL`.
#' @param free_pairs Pair ids (`"lhs_rhs"`) to free under
#'   `mode = "equal_except"`.
#' @param label_prefix The covariance label prefix (e.g. `"c"`, `"phi"`).
#' @param explicit_labels_when_free See [.parameter_label_engine()].
#' @return A character vector of covariance lines, one per row of `covars`.
#' @keywords internal
.generate_covariances <- function(covars,
                                   mode,
                                   block_id,
                                   free_pairs,
                                   label_prefix,
                                   explicit_labels_when_free = TRUE) {

  out <- character(nrow(covars))

  for (i in seq_len(nrow(covars))) {
    lhs <- covars$lhs[i]
    rhs <- covars$rhs[i]
    pair_id <- paste0(lhs, "_", rhs)

    info <- .parameter_label_engine(
      item          = pair_id,
      item_index    = i,
      block_id      = block_id,
      mode          = mode,
      scale_id      = "none",
      label_prefix  = label_prefix,
      explicit_labels_when_free = explicit_labels_when_free
    )

    out[i] <- switch(
      info$type,
      fixed_zero = paste0("  ", lhs, " ~~ 0*", rhs),
      free_na    = paste0("  ", lhs, " ~~ ", rhs),
      fixed      = paste0("  ", lhs, " ~~ ", info$label, "*", rhs),
      group_free = paste0("  ", lhs, " ~~ ", info$label, "*", rhs),
      stop("[.generate_covariances] Unknown parameter type: ", info$type, call. = FALSE)
    )
  }

  out
}


#' Generate an effects-coding sum constraint
#'
#' Builds the `l1 + l2 + ... + ln == <target>` constraint line that
#' identifies a factor's scale via effects coding (as opposed to a marker
#' item or fixed variance).
#'
#' @param label_prefix The label prefix used for the summed parameters
#'   (e.g. `"l"` for loadings, `"i"` for intercepts).
#' @param n_items The number of items being summed.
#' @param sum_target `"n_items"` (loadings sum to the item count) or
#'   `"zero"` (intercepts sum to zero).
#' @return A string with a leading blank line and comment, ready to insert
#'   via [.insert_global()].
#' @keywords internal
.generate_effects_coded_constraint <- function(
    label_prefix,
    n_items,
    sum_target = c("n_items", "zero")
) {
  sum_target <- match.arg(sum_target)

  labels <- paste0(label_prefix, seq_len(n_items))
  rhs <- if (sum_target == "n_items") n_items else 0

  constraint_line <- paste0("  ", paste(labels, collapse = " + "), " == ", rhs)
  paste0("\n", "#Effects-coded Constraints \n", constraint_line)
}


#' Generate a factor's latent-mean line
#' @inheritParams .parameter_engine_mv
#' @param factor_name The factor whose mean line to generate.
#' @return A single `<factor> ~ ...*1` string.
#' @keywords internal
.generate_factor_mean <- function(
    factor_name,
    mode,
    block_id = NULL,
    group_name = NULL,
    groups_to_fix = NULL,
    label_prefix = "m"
) {

  spec <- .parameter_engine_mv(
    mode          = mode,
    block_id      = block_id,
    group_name    = group_name,
    groups_to_fix = groups_to_fix,
    label_prefix  = label_prefix
  )

  if (spec$fixed) {
    return(paste0("  ", factor_name, " ~ ", spec$fixed_value, "*1"))
  }
  if (!is.null(spec$label)) {
    return(paste0("  ", factor_name, " ~ ", spec$label, "*1"))
  }
  paste0("  ", factor_name, " ~ NA*1")
}


#' Generate a factor's latent-variance line
#' @inheritParams .parameter_engine_mv
#' @param factor_name The factor whose variance line to generate.
#' @return A single `<factor> ~~ ...*<factor>` string.
#' @keywords internal
.generate_factor_variance <- function(
    factor_name,
    mode,
    block_id = NULL,
    group_name = NULL,
    groups_to_fix = NULL,
    label_prefix = "v"
) {

  spec <- .parameter_engine_mv(
    mode          = mode,
    block_id      = block_id,
    group_name    = group_name,
    groups_to_fix = groups_to_fix,
    label_prefix  = label_prefix
  )

  if (spec$fixed) {
    return(paste0("  ", factor_name, " ~~ ", spec$fixed_value, "*", factor_name))
  }
  if (!is.null(spec$label)) {
    return(paste0("  ", factor_name, " ~~ ", spec$label, "*", factor_name))
  }
  paste0("  ", factor_name, " ~~ NA*", factor_name)
}


#' Every unordered pair drawn from a set of variables
#' @param factors A character vector of variable (item or factor) names.
#' @return A data frame with `lhs`/`rhs` character columns, one row per
#'   unordered pair.
#' @keywords internal
.make_factor_pairs <- function(factors) {
  cmb <- utils::combn(factors, 2)
  data.frame(
    lhs = cmb[1, ],
    rhs = cmb[2, ],
    stringsAsFactors = FALSE
  )
}


#' Extract every factor name defined by `=~` in a model string
#' @param model_string A lavaan model-syntax string.
#' @return A character vector of unique factor (latent variable) names.
#' @keywords internal
.extract_factor_names <- function(model_string) {
  lines <- unlist(strsplit(model_string, "\n", fixed = TRUE))

  is_loading <- grepl("=~", lines)

  lhs <- sub("\\s*=~.*$", "", lines[is_loading])
  lhs <- trimws(lhs)

  unique(lhs)
}
