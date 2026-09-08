# Block-parsing engine for the lavaan-syntax-building toolkit ----------------
#
# build_model() (see build_model.R) assembles a lavaan model string by
# repeatedly editing it: strip out old parameter lines, write new ones, and
# do that once per "block" of the model string (once per group, once per
# level, or once for the whole thing when there's no grouping/level structure
# at all). The functions here are that block machinery:
#
#   - group_syntax() / level_syntax() wrap a model string in "group: <x>" /
#     "level: <x>" headers so lavaan's own multi-group/multilevel syntax can
#     be built on top of it.
#   - parse_single_block() / parse_group_blocks() / parse_level_blocks() cut
#     a model string back into blocks (a "prelude" plus one block per group
#     or level).
#   - .map_blocks() re-parses a string, lets a callback rewrite each block's
#     lines, and reassembles the result; it's what every set_*() function in
#     lavaan_syntax_setters.R is built on.
#   - .insert_global() adds lines that apply to the whole model (e.g. an
#     effects-coding constraint) rather than to one block.
#   - .assemble_model() turns a list of blocks back into a single string.


#' Format a group/level label for a lavaan block header
#'
#' lavaan's block parser accepts a bare label only when it is a plain
#' identifier or number; anything else (a hyphen, a space) is rejected with
#' "missing or invalid number/label". Real grouping variables routinely hold
#' such values — "Grant-White" in HolzingerSwineford1939, for one — so those
#' get wrapped in double quotes, which lavaan does accept.
#'
#' @param x A single group or level label.
#' @return The label, quoted if it needs to be.
#' @noRd
.block_label <- function(x) {
  x <- as.character(x)
  if (grepl("^[A-Za-z0-9._]+$", x)) x else paste0('"', x, '"')
}


#' Wrap a model string in per-group lavaan syntax
#'
#' Repeats `model_string` once per element of `groups`, each copy prefixed
#' with lavaan's `group: <name>` header. Used by [build_model()] when
#' `type = "groups"` to turn a single-group skeleton into the multi-group
#' block structure that [parse_group_blocks()] expects.
#'
#' @param model_string A lavaan model-syntax string (possibly empty).
#' @param levels A character (or coercible) vector of group names.
#'
#' @return A single string with one `group: <name>` block per element of
#'   `levels`, separated by newlines.
#'
#' @examples
#' cat(group_syntax("", c("male", "female")))
#'
#' @seealso [level_syntax()] for the multilevel equivalent.
#' @family lavaan_syntax
#' @export
group_syntax <- function(model_string, levels) {

  grouped_syntax <- vector("list", length(levels))
  names(grouped_syntax) <- levels

  for (i in seq_along(levels)) {
    group_i <- paste("group: ", .block_label(levels[i]), "\n", model_string)
    grouped_syntax[levels[i]] <- group_i
  }

  output <- do.call(c, grouped_syntax)
  paste(output, collapse = "\n")
}


#' Wrap a model string in per-level lavaan syntax
#'
#' The multilevel equivalent of [group_syntax()]: repeats `model_string`
#' once per element of `levels`, each copy prefixed with lavaan's
#' `level: <name>` header. Used by [build_model()] when `type = "levels"`.
#'
#' @param model_string A lavaan model-syntax string (possibly empty).
#' @param levels A vector of level identifiers (e.g. `c(1, 2)` for a
#'   two-level model, or `c("within", "between")`).
#'
#' @return A single string with one `level: <name>` block per element of
#'   `levels`, separated by newlines.
#'
#' @examples
#' cat(level_syntax("", c(1, 2)))
#'
#' @seealso [group_syntax()] for the multi-group equivalent.
#' @family lavaan_syntax
#' @export
level_syntax <- function(model_string, levels) {

  level_syn <- vector("list", length(levels))
  names(level_syn) <- levels

  for (i in seq_along(levels)) {
    level_i <- paste("level: ", .block_label(levels[i]), "\n", model_string)
    level_syn[levels[i]] <- level_i
  }

  output <- do.call(c, level_syn)
  paste(output, collapse = "\n")
}


#' Parse a single-group, single-level model string into one block
#'
#' The trivial parser: treats the entire `model_string` as one block. Used
#' as the `parser` argument to [build_model()] and the `set_*()` functions
#' when `type = "single"` (no `group:`/`level:` structure at all).
#'
#' @param model_string A lavaan model-syntax string. Must not contain
#'   `group:` lines (use [parse_group_blocks()] for those).
#'
#' @return A length-1 list. Its one element has `lines` (the split-by-line
#'   model string) and `indices` (`list(type = "single", block_id = 1)`).
#'
#' @examples
#' parse_single_block("f1 =~ x1 + x2 + x3")
#'
#' @seealso [parse_group_blocks()], [parse_level_blocks()]
#' @family lavaan_syntax
#' @export
parse_single_block <- function(model_string) {
  if (length(model_string) != 1L) {
    stop(
      "parse_single_block(): model_string must be a single string.\n",
      "Length detected: ", length(model_string),
      call. = FALSE
    )
  }

  lines <- strsplit(model_string, "\n", fixed = TRUE)[[1]]

  if (any(grepl("^\\s*group:", lines))) {
    stop(
      "parse_single_block(): 'group:' syntax detected.\n",
      "Use parse_group_blocks() for multi-group models.",
      call. = FALSE
    )
  }

  list(list(
    lines   = lines,
    indices = list(type = "single", block_id = 1)
  ))
}


#' Parse a multi-group model string into blocks
#'
#' Splits `model_string` at each `group: <name>` line into a leading
#' "prelude" block (anything before the first `group:` line, if present)
#' followed by one block per group. Used as the `parser` argument to
#' [build_model()] and the `set_*()` functions when `type = "groups"`.
#'
#' @param model_string A lavaan model-syntax string containing at least one
#'   `group: <name>` line.
#'
#' @return A list of blocks. Each block has `lines` (its lines of syntax)
#'   and `indices`, which is either `list(type = "prelude")` or
#'   `list(type = "group", block_id = <int>, group_name = <chr>)`.
#'
#' @examples
#' parse_group_blocks(group_syntax("f1 =~ x1 + x2 + x3", c("a", "b")))
#'
#' @seealso [parse_single_block()], [parse_level_blocks()]
#' @family lavaan_syntax
#' @export
parse_group_blocks <- function(model_string) {
  if (length(model_string) != 1L) {
    stop("parse_group_blocks(): model_string must be a single string.\n",
         "Length detected: ", length(model_string), call. = FALSE)
  }

  lines  <- strsplit(model_string, "\n", fixed = TRUE)[[1]]
  starts <- which(grepl("^\\s*group:", lines))

  if (!length(starts)) {
    stop(
      "parse_group_blocks(): No 'group:' blocks detected in model syntax.\n",
      "This parser should only be used for multi-group models.\n",
      "If this is a single-group model, use parse_single_block() instead.",
      call. = FALSE
    )
  }

  ends <- c(starts[-1] - 1L, length(lines))
  blocks <- list()

  if (starts[1] > 1L) {
    blocks <- c(blocks, list(list(
      lines   = lines[1L:(starts[1] - 1L)],
      indices = list(type = "prelude")
    )))
  }

  for (i in seq_along(starts)) {
    rng <- starts[i]:ends[i]
    block_lines <- lines[rng]
    group_name <- sub("^\\s*group:\\s*(.*)\\s*$", "\\1", block_lines[1])
    # Labels that are not bare identifiers are written quoted (see
    # .block_label()); strip the quotes so group_name still matches the
    # plain names a caller passes to groups_to_fix.
    group_name <- gsub('^"|"$', "", trimws(group_name))

    blocks <- c(blocks, list(list(
      lines   = block_lines,
      indices = list(
        type       = "group",
        block_id   = i,
        group_name = group_name
      )
    )))
  }

  blocks
}


#' Parse a multilevel model string into blocks
#'
#' The multilevel equivalent of [parse_group_blocks()]: splits `model_string`
#' at each `level: <name>` line into a leading "prelude" block followed by
#' one block per level. Used as the `parser` argument to [build_model()] and
#' the `set_*()` functions when `type = "levels"`.
#'
#' @param model_string A lavaan model-syntax string containing at least one
#'   `level: <name>` line.
#'
#' @return A list of blocks. Each block has `lines` (its lines of syntax)
#'   and `indices`, which is either `list(type = "prelude")` or
#'   `list(type = "level", block_id = <int>, level_name = <chr>)`.
#'
#' @examples
#' parse_level_blocks(level_syntax("f1 =~ x1 + x2 + x3", c(1, 2)))
#'
#' @seealso [parse_single_block()], [parse_group_blocks()]
#' @family lavaan_syntax
#' @export
parse_level_blocks <- function(model_string) {
  if (length(model_string) != 1L) {
    stop(
      "parse_level_blocks(): model_string must be a single string.\n",
      "Length detected: ", length(model_string),
      call. = FALSE
    )
  }

  lines  <- strsplit(model_string, "\n", fixed = TRUE)[[1]]
  lines  <- trimws(lines)
  lines  <- lines[lines != ""]

  starts <- which(grepl("^level:", lines, ignore.case = TRUE))

  if (!length(starts)) {
    stop(
      "parse_level_blocks(): No 'level:' blocks detected in model syntax.\n",
      "This parser should only be used for multilevel models.",
      call. = FALSE
    )
  }

  ends   <- c(starts[-1] - 1L, length(lines))
  blocks <- list()

  if (starts[1] > 1L) {
    blocks <- c(blocks, list(list(
      lines   = lines[1L:(starts[1] - 1L)],
      indices = list(type = "prelude")
    )))
  }

  for (i in seq_along(starts)) {
    rng <- starts[i]:ends[i]
    block_lines <- lines[rng]

    level_name <- sub(
      "^level:\\s*(.*)$", "\\1", block_lines[1],
      ignore.case = TRUE
    )

    blocks <- c(blocks, list(list(
      lines   = block_lines,
      indices = list(
        type       = "level",
        block_id   = i,
        level_name = level_name
      )
    )))
  }

  blocks
}


#' Reassemble parsed blocks into a single model string
#'
#' The inverse of the `parse_*_block(s)()` functions: flattens a list of
#' blocks (each with a `lines` element) back into one newline-joined string.
#'
#' @param blocks A list of blocks as returned by [parse_single_block()],
#'   [parse_group_blocks()], or [parse_level_blocks()].
#'
#' @return A single character string.
#' @keywords internal
.assemble_model <- function(blocks) {
  out_lines <- unlist(lapply(blocks, function(b) b$lines), use.names = FALSE)
  paste(out_lines, collapse = "\n")
}


#' Rewrite every block of a model string
#'
#' Parses `model_string` with `parser`, applies `FUN` to each block's lines
#' in turn, and reassembles the result. This is the core engine every
#' `set_*()` function in lavaan_syntax_setters.R is built on: they each
#' supply a `FUN` that strips out one kind of parameter line (loadings,
#' intercepts, ...) and writes new ones for that block.
#'
#' @param model_string A lavaan model-syntax string.
#' @param parser One of [parse_single_block()], [parse_group_blocks()], or
#'   [parse_level_blocks()] (unquoted function, not a string).
#' @param FUN A function of `(block, indices)` returning the rewritten
#'   character vector of lines for that block. `block` is the block's lines;
#'   `indices` is its metadata list (see the `parse_*` functions).
#'
#' @return The reassembled model string.
#' @keywords internal
.map_blocks <- function(model_string, parser, FUN) {
  if (!is.function(parser)) stop(".map_blocks(): `parser` must be a function.", call. = FALSE)
  if (!is.function(FUN))    stop(".map_blocks(): `FUN` must be a function.", call. = FALSE)

  blocks <- parser(model_string)

  for (i in seq_along(blocks)) {
    blocks[[i]]$lines <- FUN(
      block   = blocks[[i]]$lines,
      indices = blocks[[i]]$indices
    )

    if (!is.character(blocks[[i]]$lines)) {
      stop(".map_blocks(): FUN must return a character vector of lines.", call. = FALSE)
    }
  }

  .assemble_model(blocks)
}


#' Insert lines that apply to the whole model, not to one block
#'
#' Adds `lines` either to the model's prelude (creating one if none exists)
#' or as a trailing "postlude" block. Used by the `set_*()` functions to
#' attach model-wide constraints — e.g. the `l1 + l2 + l3 == 3` sum
#' constraint an effects-coded loading identification needs — which belong
#' once per model rather than once per group/level block.
#'
#' @param model_string A lavaan model-syntax string.
#' @param parser One of [parse_single_block()], [parse_group_blocks()], or
#'   [parse_level_blocks()].
#' @param lines A character vector of lines to insert.
#' @param where `"prelude"` (default) to prepend before the first block, or
#'   `"postlude"` to append after the last block.
#'
#' @return The reassembled model string with `lines` inserted.
#' @keywords internal
.insert_global <- function(model_string, parser, lines,
                            where = c("prelude", "postlude")) {

  where <- match.arg(where)
  blocks <- parser(model_string)

  if (where == "prelude") {

    prelude_idx <- which(vapply(
      blocks,
      function(b) identical(b$indices$type, "prelude"),
      logical(1)
    ))

    if (length(prelude_idx) == 0L) {
      blocks <- c(
        list(list(lines = lines, indices = list(type = "prelude"))),
        blocks
      )
    } else {
      blocks[[prelude_idx]]$lines <- c(blocks[[prelude_idx]]$lines, lines)
    }

  } else {
    blocks <- c(
      blocks,
      list(list(lines = lines, indices = list(type = "postlude")))
    )
  }

  .assemble_model(blocks)
}
