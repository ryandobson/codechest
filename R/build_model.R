# ============================================================
# build_model.R
#
# The top of the lavaan syntax toolkit: build_model() composes the
# set_*() functions into a complete model string.
#
# Order matters. For each factor it writes loadings, then intercepts,
# then residuals, then the latent variance, then the latent mean; item
# covariances and factor covariances are written once at the end,
# because they belong to the model as a whole rather than to any one
# factor.
#
# Cross-loading items are handled by only writing an item's intercept
# and residual the first time that item is seen, since an item has one
# intercept and one residual no matter how many factors it loads on.
# ============================================================


#' Build a lavaan model string with invariance constraints
#'
#' Assembles a complete lavaan model syntax string from a list of factors and
#' their items, applying equality constraints across groups or levels. This is
#' the top-level entry point of the syntax toolkit: it calls [set_loadings()],
#' [set_intercepts()], [set_residuals()], [set_variances()], [set_means()],
#' [set_item_covariances()], and [set_factor_covariances()] in the right order
#' and hands back a string ready for `lavaan::cfa()`.
#'
#' @details
#' The `*_mode` arguments accept either a single value applied to every factor,
#' or a factor-keyed named list for per-factor control. That is what makes
#' partial invariance expressible: you can hold one factor's loadings equal
#' while freeing another's.
#'
#' Each factor's parameter labels are prefixed with a factor id (`f1_`, `f2_`,
#' ...) so labels never collide across factors. Supply `factor_ids` to control
#' those prefixes yourself.
#'
#' Items that load on more than one factor get their intercept and residual
#' written only once, when the item is first encountered. An item has a single
#' intercept and a single residual regardless of how many factors it loads on,
#' so writing them per factor would produce duplicate lines that lavaan
#' rejects.
#'
#' @param factors A named list mapping each factor name to a character vector
#'   of its items, e.g. `list(f1 = c("x1", "x2", "x3"))`.
#' @param type Model structure: `"single"` (one block), `"groups"`
#'   (multi-group, requires `groups`), or `"levels"` (multilevel). The value
#'   `"factors"` is accepted by the argument matcher but is not implemented
#'   and will error — it is reserved for a planned cross-factor constraint
#'   mode.
#' @param groups Character vector of group names. Required when
#'   `type = "groups"`.
#' @param levels Numeric vector of level ids for `type = "levels"`.
#'   Default `c(1, 2)`.
#' @param variances_mode Latent variance identification, per factor. One of
#'   `"first_one"`, `"all_one"`, `"groups_one"`, `"none"`, `"all_equal"`.
#' @param means_mode Latent mean identification, per factor. One of
#'   `"first_zero"`, `"all_zero"`, `"groups_zero"`, `"none"`, `"all_equal"`.
#' @param var_groups_to_fix Groups whose latent variance to fix, used with
#'   `variances_mode = "groups_one"`.
#' @param mean_groups_to_fix Groups whose latent mean to fix, used with
#'   `means_mode = "groups_zero"`.
#' @param loadings_mode Loading constraint, per factor. One of `"equal_all"`,
#'   `"equal_except"`, `"free_all"`.
#' @param intercepts_mode Intercept constraint for the whole model.
#' @param residuals_mode Residual variance constraint for the whole model.
#' @param item_covars_mode Constraint applied to item covariances.
#' @param factor_covars_mode Constraint applied to factor covariances. One of
#'   `"free_all"`, `"equal_all"`, `"equal_except"`, `"fixed_zero"`.
#' @param factor_free_pairs Factor pairs left free when
#'   `factor_covars_mode = "equal_except"`.
#' @param factor_ids Character vector of label prefixes, one per factor.
#'   Generated automatically when `NULL`.
#' @param free_loadings,free_intercepts,free_residuals Items to leave free
#'   under the corresponding `"equal_except"` mode.
#' @param item_covars Named character vector of item pairs to covary, e.g.
#'   `c(x1 = "x2")`.
#' @param free_item_covars Item pairs left free when
#'   `item_covars_mode = "equal_except"`.
#' @param loadings_scale_id Loading scale identification, per factor:
#'   `"none"`, `"marker"`, or `"effects_coded"`.
#' @param loadings_marker_items Named list of marker items keyed by factor,
#'   required when `loadings_scale_id = "marker"`.
#' @param intercepts_scale_id Intercept scale identification: `"none"`,
#'   `"marker"`, or `"effects_coded"`.
#' @param intercepts_marker_items Named list of intercept marker items.
#' @param label_prefixes Named list of label prefixes for each parameter type.
#' @param verbose Print a summary of the constraints as they are applied.
#'
#' @return A lavaan model syntax string.
#'
#' @examples
#' # a one-factor model in a single block
#' ms <- build_model(factors = list(f1 = c("x1", "x2", "x3")))
#' cat(ms)
#'
#' # the same model across two groups, loadings held equal
#' mg <- build_model(
#'   factors = list(f1 = c("x1", "x2", "x3")),
#'   type    = "groups",
#'   groups  = c("a", "b")
#' )
#' cat(mg)
#'
#' @family lavaan syntax
#' @seealso [set_loadings()] and the other setters this composes.
#' @export
build_model <- function(factors,
                        type = c("single", "groups", "levels", "factors"),
                        groups = NULL,
                        levels = c(1, 2),
                        # --- factor-level options ---
                        variances_mode = c("first_one", "all_one",
                                           "groups_one", "none", "all_equal"),
                        means_mode = c("first_zero", "all_zero",
                                       "groups_zero", "none", "all_equal"),
                        var_groups_to_fix = NULL,
                        mean_groups_to_fix = NULL,
                        # --- item-level options ---
                        loadings_mode = c("equal_all", "equal_except", "free_all"),
                        intercepts_mode = c("equal_all", "equal_except", "free_all"),
                        residuals_mode = c("equal_all", "equal_except", "free_all"),
                        item_covars_mode = c("equal_all", "equal_except", "free_all"),
                        # --- latent covariance options ---
                        factor_covars_mode = c("free_all", "equal_all",
                                               "equal_except", "fixed_zero"),
                        factor_free_pairs = NULL,
                        factor_ids = NULL,
                        # --- flexibility arguments ---
                        free_loadings = NULL,
                        free_intercepts = NULL,
                        free_residuals = NULL,
                        item_covars = NULL,
                        free_item_covars = NULL,
                        # --- identification options ---
                        loadings_scale_id = c("none", "marker", "effects_coded"),
                        loadings_marker_items = NULL,
                        intercepts_scale_id = c("none", "marker", "effects_coded"),
                        intercepts_marker_items = NULL,
                        # --- labeling and verbosity ---
                        label_prefixes = list(
                          loadings    = "l",
                          intercepts  = "i",
                          residuals   = "r",
                          variances   = "v",
                          means       = "m",
                          item_covs   = "c",
                          factor_covs = "phi"
                        ),
                        verbose = FALSE) {

  # Deduplicated: a cross-loading item appears under every factor it loads
  # on, and all_items is used to decide which covariance lines to clear.
  # Leaving duplicates in makes .make_factor_pairs() emit self-pairs like
  # (x1, x1), which then match — and silently delete — that item's residual
  # variance line.
  all_items    <- unique(do.call(c, factors))
  factor_names <- names(factors)

  # Per-factor options may arrive as a scalar, a named vector, or a named
  # list; normalize them all to a factor-keyed list before use.
  loadings_mode     <- .normalize_factor_arg(loadings_mode, factor_names,
                                             "loadings_mode")
  variances_mode    <- .normalize_factor_arg(variances_mode, factor_names,
                                             "variances_mode")
  means_mode        <- .normalize_factor_arg(means_mode, factor_names,
                                             "means_mode")
  loadings_scale_id <- .normalize_factor_arg(loadings_scale_id, factor_names,
                                             "loadings_scale_id")

  var_groups_to_fix <- .normalize_factor_groups_to_fix(
    var_groups_to_fix, factor_names, "var_groups_to_fix")

  mean_groups_to_fix <- .normalize_factor_groups_to_fix(
    mean_groups_to_fix, factor_names, "mean_groups_to_fix")

  for (fac in factor_names) {

    if (loadings_scale_id[[fac]] == "marker" &&
        (is.null(loadings_marker_items) || is.null(loadings_marker_items[[fac]]))) {
      stop("[build_model] Factor '", fac,
           "' uses marker identification but no marker item was supplied.",
           call. = FALSE)
    }

    if (!is.null(var_groups_to_fix[[fac]]) && variances_mode[[fac]] == "none") {
      warning("[build_model] var_groups_to_fix specified for factor '", fac,
              "' but variances_mode is 'none'. Ignoring.", call. = FALSE)
    }

    if (!is.null(mean_groups_to_fix[[fac]]) && means_mode[[fac]] == "none") {
      warning("[build_model] mean_groups_to_fix specified for factor '", fac,
              "' but means_mode is 'none'. Ignoring.", call. = FALSE)
    }
  }

  type                <- match.arg(type)
  intercepts_mode     <- match.arg(intercepts_mode)
  intercepts_scale_id <- match.arg(intercepts_scale_id)
  residuals_mode      <- match.arg(residuals_mode)
  item_covars_mode    <- match.arg(item_covars_mode)
  factor_covars_mode  <- match.arg(factor_covars_mode)

  if (any(type == "groups") && is.null(groups)) {
    stop("'type' was set to 'groups' for at least one factor, but 'groups' ",
         "was NULL.\nPlease provide a character vector of group names to ",
         "'groups' or switch the model type.", call. = FALSE)
  }

  if (intercepts_scale_id == "marker" && is.null(intercepts_marker_items)) {
    warning("'intercepts_scale_id' was set to 'marker' but no ",
            "'intercepts_marker_items' were supplied.", call. = FALSE)
  }

  # Only model-wide settings are known before the factor loop; anything
  # per-factor is reported inside it, once the values exist.
  if (isTRUE(verbose)) {
    message("\n[build_model]")
    message("  Type: ", type)
    if (!is.null(groups)) {
      message("  Groups: ", paste(groups, collapse = ", "))
    }
    message("  Factors: ", paste(factor_names, collapse = ", "))
    message("  Intercepts mode: ", intercepts_mode,
            " (scale_id = ", intercepts_scale_id, ")")
    message("  Residuals mode: ", residuals_mode)
    message("  Item covariances mode: ", item_covars_mode)
    message("  Factor covariances mode: ", factor_covars_mode)
    if (!is.null(intercepts_marker_items)) {
      message("  Intercept markers: ",
              paste(intercepts_marker_items, collapse = ", "))
    }
  }

  # Give each factor its own label prefix so labels cannot collide.
  if (is.null(factor_ids) && length(factor_names) > 1) {
    factor_ids <- paste0("f", seq_along(factor_names))
    factor_ids <- paste0(factor_ids, "_")
  } else if (is.null(factor_ids) && length(factor_names) == 1) {
    factor_ids <- ""
  }

  if (!is.null(factor_ids) && typeof(factor_ids) != "character") {
    stop("You provided 'factor_ids', but it was not a character vector. ",
         "Please provide a character vector of factor prefixes.",
         call. = FALSE)
  } else if (!is.null(factor_ids) &&
             length(factor_names) != length(factor_ids)) {
    # This has to be fatal. Indexing past the end yields NA, and every
    # factor that falls off the end gets the same "NA" prefix, so two
    # factors end up sharing labels — which lavaan reads as an equality
    # constraint between them. The model fits and says nothing.
    stop("You provided ", length(factor_ids), " 'factor_ids' for ",
         length(factor_names), " factors. Supply one prefix per factor, ",
         "or leave 'factor_ids' NULL to have them generated.",
         call. = FALSE)
  }

  parser <- switch(
    type,
    "single" = parse_single_block,
    "groups" = parse_group_blocks,
    "levels" = parse_level_blocks
  )

  model_string <- ""

  model_string <- switch(
    type,
    single = model_string,
    groups = group_syntax(model_string, groups),
    levels = level_syntax(model_string, levels),
    stop("Unknown parser 'type', please use type = 'single', 'groups', or ",
         "'levels'.", call. = FALSE)
  )

  for (i in seq_along(factor_names)) {

    factor_name <- factor_names[i]
    items       <- factors[[i]]
    factor_id   <- factor_ids[i]

    factor_label_prefixes <- list(
      loadings   = paste0(factor_id, label_prefixes$loadings),
      intercepts = paste0(factor_id, label_prefixes$intercepts),
      residuals  = paste0(factor_id, label_prefixes$residuals),
      variances  = paste0(factor_id, label_prefixes$variances),
      means      = paste0(factor_id, label_prefixes$means),
      item_covs  = paste0(factor_id, label_prefixes$item_covs)
    )

    loadings_mode_i <- match.arg(
      loadings_mode[[factor_name]],
      choices = c("equal_all", "equal_except", "free_all"))

    loadings_scale_id_i <- match.arg(
      loadings_scale_id[[factor_name]],
      choices = c("none", "marker", "effects_coded"))

    variances_mode_i <- match.arg(
      variances_mode[[factor_name]],
      choices = c("first_one", "all_one", "groups_one", "none", "all_equal"))

    means_mode_i <- match.arg(
      means_mode[[factor_name]],
      choices = c("first_zero", "all_zero", "groups_zero", "none", "all_equal"))

    if (isTRUE(verbose)) {
      message("\n  Latent factor: ", factor_name)
      message("    Items: ", paste(items, collapse = ", "))
      message("    Loadings mode: ", loadings_mode_i,
              " (scale_id = ", loadings_scale_id_i, ")")
      message("    Variances mode: ", variances_mode_i)
      message("    Means mode: ", means_mode_i)
      if (!is.null(var_groups_to_fix[[factor_name]])) {
        message("    Variances fixed in: ",
                paste(var_groups_to_fix[[factor_name]], collapse = ", "))
      }
      if (!is.null(mean_groups_to_fix[[factor_name]])) {
        message("    Means fixed in: ",
                paste(mean_groups_to_fix[[factor_name]], collapse = ", "))
      }
      if (!is.null(loadings_marker_items[[factor_name]])) {
        message("    Loading marker: ", loadings_marker_items[[factor_name]])
      }
    }

    # --- 1. Factor loadings ---
    model_string <- set_loadings(
      model_string = model_string,
      parser       = parser,
      factor_name  = factor_name,
      items        = items,
      mode         = loadings_mode_i,
      free_items   = free_loadings,
      scale_id     = loadings_scale_id_i,
      marker_items = loadings_marker_items,
      label_prefix = factor_label_prefixes$loadings
    )

    # An item has one intercept and one residual however many factors it
    # loads on, so drop items already covered by an earlier factor before
    # writing those two.
    if (i > 1) {
      prior_items <- do.call(c, factors[1:(i - 1)])
      items <- items[!items %in% prior_items]
    }

    # --- 2. Intercepts ---
    model_string <- set_intercepts(
      model_string = model_string,
      parser       = parser,
      factor_name  = factor_name,
      items        = items,
      mode         = intercepts_mode,
      free_items   = free_intercepts,
      scale_id     = intercepts_scale_id,
      marker_items = intercepts_marker_items,
      label_prefix = factor_label_prefixes$intercepts
    )

    # --- 3. Residual variances ---
    model_string <- set_residuals(
      model_string = model_string,
      parser       = parser,
      items        = items,
      mode         = residuals_mode,
      free_items   = free_residuals,
      label_prefix = factor_label_prefixes$residuals
    )

    # --- 4. Latent variances ---
    model_string <- set_variances(
      model_string   = model_string,
      parser         = parser,
      factor_name    = factor_name,
      variances_mode = variances_mode_i,
      groups_to_fix  = var_groups_to_fix[[factor_name]],
      label_prefix   = factor_label_prefixes$variances
    )

    # --- 5. Latent means ---
    model_string <- set_means(
      model_string  = model_string,
      parser        = parser,
      factor_name   = factor_name,
      means_mode    = means_mode_i,
      groups_to_fix = mean_groups_to_fix[[factor_name]],
      label_prefix  = factor_label_prefixes$means
    )
  }

  # Item and factor covariances belong to the model as a whole, so they are
  # written once rather than per factor.

  # --- 6. Item-level covariances ---
  if (!is.null(item_covars)) {
    model_string <- set_item_covariances(
      model_string = model_string,
      parser       = parser,
      items        = all_items,
      item_covars  = item_covars,
      mode         = item_covars_mode,
      free_pairs   = free_item_covars,
      label_prefix = label_prefixes$item_covs
    )
  }

  # --- 7. Latent covariances ---
  if (length(factors) > 1) {
    model_string <- set_factor_covariances(
      model_string = model_string,
      parser       = parser,
      factors      = factor_names,
      free_pairs   = factor_free_pairs,
      mode         = factor_covars_mode,
      label_prefix = label_prefixes$factor_covs
    )
  }

  if (isTRUE(verbose)) message("\n  All constraints applied successfully.")

  model_string
}
