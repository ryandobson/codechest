

#' Rename Rows in Selected Data Frame Columns
#'
#' This function replaces values in specified columns of a data frame using a
#' named vector of new names. It is useful for making output tables more
#' readable (e.g., converting variable codes to human-readable labels) without
#' renaming variables in the raw dataset.
#'
#' @param df A data frame. Typically an output table where row values need to be
#' renamed (e.g., descriptives or model output).
#' @param rename_vector A named character vector of the form
#'   \code{c("New Name" = "Old Name", ...)}. The old values are matched and
#' replaced with the new names.
#' @param columns_to_rename Optional character vector of column names to search
#' for values to rename. If \code{NULL} (default), all columns in \code{df} are
#' considered.
#'
#' @details
#' - The function loops through each row and column, checking if a value matches
#'   an entry in \code{rename_vector}. If so, the value is replaced by the
#'   corresponding new name.
#' - For large data frames, this approach may be slow because it uses nested
#'   for-loops. If speed becomes an issue, consider vectorized or
#'   \pkg{data.table}-based alternatives.
#' - To avoid unintended changes, you can specify \code{columns_to_rename} so
#'   only selected columns are edited.
#'
#' @return A data frame with specified row values replaced according to
#' \code{rename_vector}.
#'
#' @examples
#' df <- data.frame(var1 = c("x1", "x2", "x3"),
#'                  var2 = c("y1", "y2", "y3"))
#' renames <- c("Item A" = "x1", "Item B" = "y2")
#'
#' rename_rows(df, renames)
#'
#' # Restrict renaming to var1 only
#' rename_rows(df, renames, columns_to_rename = "var1")
#'
#' @seealso \code{\link[dplyr]{recode}}, \code{\link[data.table]{set}}
#'
#' @export

rename_rows <- function(df, rename_vector, columns_to_rename = NULL) {

  #> General Coding Note (8/4/2025):
  #> I designed this function to nicely use for loops. However, for loops, especially
  #> in R are not very fast and not optimized for some things. For most use cases of
  #> this function, that won't be much of an issue. However, I could vectorize this
  #> function. ChatGPT recommended using data.table
  #> for lighting fast implementation. If I have speed issues in the future I can
  #> consider rewriting this function. It does the trick for now though.


  #> Possible Function Improvements (8/4/2025):
  #> 1. Speed: (see above note)
  #> 2. Tracking:
  #>  I could track what things this function edits before to unwanted errors. For
  #>  example, I could track which columns were updated and what columns were not
  #>  This would ensure that only the columns I expected to be updated were updated.
  #>  I can also just supply specific column names to avoid issues, though.


  # df = A dataframe, typically formatted as an output table. I can supply the
  # full dataframe here, or I can supply only the subset of the df with the columns
  # to rename

  # columns_to_rename = a vector of column names which have rows with values to
  #be renamed.
  #> NOTE: If I do not supply this, it will iterate across all of the columns in
  #the dataframe. For smaller df's this does not cause any issues and it performs
  #quickly. If I have a very large df, I should probably only use the necessary
  #columns.

  # rename_vector = A named vector of the format: new name = old name

  #> Example Usage: I frequently create output tables where columns end up containing
  #> different variable names. A descriptives table from the psych package output
  #> is a great example of that. In many cases I have different variable names
  #> referenced in R that are not pretty in output. Thus, I want to rename those
  #> variables. This also happens in model output from statistical models frequently too.
  #> Many times, I don't want to rename these variables before running the model
  #> as this is tedious and not easy for selection. So, this function is designed
  #> to easily handle renaming variables as long as I specify a vector of named
  #> variables where it can rename variables based on that.

  #> Grabs all columns from the dataframe if I don't supply specific columns from
  #> a different vector
  if (is.null(columns_to_rename)) {
    columns_to_rename <- colnames(df)
  }

#iterate over each column to rename
  for (col in columns_to_rename) {

   #iterate over each row of the relevant column in the df to rename
    for (i in 1:nrow(df)) {

      #grab the index of the row and the current column and see if there are
      #any elements of the rename_vector that match that specific instance
      match_index <- match(df[i, col], rename_vector)

    #if there is a match, then grab that location and rename it.
      if (!is.na(match_index)) {
        var_loc <- match(df[i, col], rename_vector) #grabs the correct location
        #of the variable to rename from list

        df[i, col] <- names(rename_vector[var_loc])
      } #end of if statement

    }#end of inner matching loop

  }#end of outer loop

  return(df)

}





#' Apply variable labels and anchors to a data frame
#'
#' This function iterates over a list of variable metadata and applies
#' descriptive labels and optional anchor value labels to matching
#' variables in a data frame. It is designed to work with
#' `haven::labelled()` so the resulting data frame is ready for
#' labelled-data workflows (e.g., exporting to SPSS/Stata or
#' value-labelled analysis in R).
#'
#' @param df A data frame containing the variables to be labelled.
#' @param variable_info A named list of metadata, where each element
#'   corresponds to a variable in `df`. Each element should itself be a
#'   list containing at least:
#'   \describe{
#'     \item{label}{A character string giving the descriptive label for the variable.}
#'     \item{anchors}{(Optional) A character string naming which entry
#'       of `anchor_catalog` to use for value labels. If `NULL` or
#'       absent, only the variable label is applied.}
#'   }
#' @param anchor_catalog A named list of anchor sets (value labels),
#'   where each entry is a named vector suitable for the `labels`
#'   argument of `haven::labelled()`. Names should correspond to the
#'   possible `anchors` values in `variable_info`.
#'
#' @return A data frame with the same structure as `df` but with
#'   variable labels (and value labels, if specified) applied using
#'   `haven::labelled`.
#'
#' @examples
#'
#' df <- data.frame(
#'   sex = c(1, 2, 1),
#'   age = c(23, 31, 27)
#' )
#'
#' variable_info <- list(
#'   sex = list(
#'     label = "Participant sex",
#'     anchors = "sex"
#'   ),
#'   age = list(
#'     label = "Participant age"
#'   )
#' )
#'
#' anchor_catalog <- list(
#'   sex = c("Male" = 1, "Female" = 2)
#' )
#'
#' labelled_df <- apply_variable_info(df, variable_info, anchor_catalog)
#' str(labelled_df)
#'
#' @export

apply_variable_info <- function(df, variable_info, anchor_catalog) {
  for (v in names(variable_info)) {
    if (!v %in% names(df)) next

    info <- variable_info[[v]]
    lab  <- info$label

    # only apply anchors if the field exists and is not NULL
    if ("anchors" %in% names(info) && !is.null(info$anchors)) {
      anc <- anchor_catalog[[info$anchors]]
      df[[v]] <- haven::labelled(df[[v]], labels = anc, label = lab)
    } else {
      df[[v]] <- haven::labelled(df[[v]], label = lab)
    }
  }
  df
}







