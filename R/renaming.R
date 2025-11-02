utils::globalVariables(c("confint", "contrasts"))


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


#' Create a mapping for a single variable's model-encoded name
#'
#' Builds a one-element named character vector mapping the original variable
#' name (the \emph{name} of the element) to the coefficient-name used by
#' \code{lmer}/\code{glmer} in model output (the \emph{value}). Handles
#' continuous variables and factors (with or without contrasts).
#'
#' @param data A data frame containing \code{var}.
#' @param var  Character scalar; the variable name in \code{data}.
#'
#' @return A named character vector of length 1, e.g. \code{c(study = "study1")}.
#'   For non-factors, this is \code{c(var = "var")}; for factors, the value
#'   reflects how the term appears in the coefficient table given the contrasts.
#'
#' @examples
#' \dontrun{
#' create_nice_name(df, "study")   # e.g., c(study = "study1")
#' create_nice_name(df, "age")     # c(age = "age")
#' }
#'
#' @importFrom stats contrasts
#' @family renaming_helpers
create_nice_name <- function(data, var) {

  #> This function operates on a single variable and is called by
  #> create_nice_names
  #> df = the dataframe where the variable originated
  #> var = the specific variable to create a naming vector for.

  #> output = naming vector of the structure:
  #> nice_name <- c("new name" = "old name")
  #> The old name is the variable name as it will be renamed in an lmer model
  #> The new name is the original variable name in the model
  #> nice_name will be a named character vector of lenghth = 1

  #grab variable for ease
  x <- data[[var]]

  #check if the variable is NOT a factor first
  if (!is.factor(x)) {
    #for variables that are not a factor, ensure I still input a named vector
    #with the same name as the variable
    new_name <- setNames(var, var)
    return(new_name)
  }#return regular variable name if it isn't.
  #> if a variable is continuous, it can't have contrasts. Contrasts can only
  #> be applied to factors.

  #if the variable is not a factor, the above note will return the regular variable
  #name.
  #if it is a factor, I can then check to see if it has a contrast.
  x_contrast <- contrasts(x)
  contrast_col_name <- colnames(x_contrast)

  #If the variable doesn't have a specific contrast setup, but is a factor and
  #the factor does not have labels for each level I get this:
  if (is.null(contrast_col_name)) {
    #create the updated name that will be listed in model output
    new_var <- paste0(var, "1")
    #create the named vector where "original_name = new_var"
    #original_var = the variable in the dataset
    #new_var = the new variable name in model output
    new_name <- setNames(new_var, var)
    return(new_name)
  }

  #This condition occurs if the contrast is not setup, but the variable is a
  #factor with labels?
  if (!is.null(contrast_col_name)) {
    new_var <- paste0(var, contrast_col_name)
    new_name <- setNames(new_var, var)
    return(new_name)
  }

  #> This condition occurs if the contrast is setup and I've named the factor
  #> levels.
  if (!is.null(contrast_col_name) & #contrast is named
      #is.element checks if the value in the first argument is contained within
      #the second element.
      !is.element(contrast_col_name, levels(x))) {
    new_var <- paste0(var, contrast_col_name)
    new_name <- setNames(new_var, var)
    return(new_name)
  }

}



#' Build model-effects parts and a name-mapping vector
#'
#' For a table of model effects (coefficients merged with CIs), split the
#' coefficient name into up to three components (e.g., for two- and three-way
#' interactions) and assemble a \emph{name-mapping} vector that maps original
#' variable names to the model-encoded names used by \code{lmer}/\code{glmer}.
#'
#' @param model_effects A data frame of effects with a \code{variable} column
#'   (e.g., coefficients merged with CIs).
#' @param fixed_effects Character vector of fixed-effect terms in the model (no bars).
#' @param rename_vec Optional pre-existing mapping vector to extend (names =
#'   original variable, values = model-encoded coefficient label).
#' @param data A data frame containing the variables referenced by \code{fixed_effects}.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{model_effects}}{The input table with extra columns \code{new_var1},
#'         \code{new_var2}, \code{new_var3} holding interaction components.}
#'   \item{\code{nice_names}}{A named character vector mapping original variable
#'         names to model-encoded coefficient names (extended by \code{rename_vec} if provided).}
#' }
#'
#' @examples
#' \dontrun{
#' out <- create_nice_names(model_effects = coef_ci,
#'                          fixed_effects = c("study","sex","study:sex"),
#'                          data = df)
#' out$nice_names
#' }
#'
#' @family renaming_helpers
create_nice_names <- function(model_effects,
                              fixed_effects,
                              rename_vec = NULL,
                              data = data) {

  #> This function is called by rename_lmer and is only semi-useful by itself.
  #> will probably never need to call this function alone, just call rename_lmer.

  #> model_effects = the table of coefficients from lmer
  #> generated from as.data.frame(model_fit$coefficients)
  #> within rename_lmer I make the above call and also add in the Wald CI's to
  #> the dataframe.
  #> fixed_effects = a character vector of all of the fixed effects in the model
  #> rename_vec = if I have a pre-specified vector of name mappings, provide this
  #> The mapping is c("new name" = "old name")
  #> If I don't provide that original vector, it will just create a new one within
  #> the function.
  #> That is fine and it will just result in a character vector of the mappings
  #> for the variables that had their names changed from the raw variable name
  #> in R to the variable name within the lmer model output.

  #> This function outputs two things:
  #> (1) model_effects - this is the table of model effects, which is not yet
  #> renamed. The table produced from this function will still have all of the
  #> interaction terms split up into different variables.
  #> (2) nice_names - this is the character vector that maps the names to update
  #> nice_names will also include the original name mapping if I supply it.
  #> that makes the renaming step simpler later.

  #duplicate object for usage within function
  me <- model_effects


  #> The code below is grabbing the variable column (which is created from rownames)
  #> supplied from lmer. It then splits up the variable column into separate pieces
  #> where
  # (1) new_var1 = main effect terms and the first string in an interaction
  # (2) new_var2 = the second string of an interaction
  # (3) new_var3 = the third string of an interaction (if there is a 3-way interaction)
  #> strings are separated by ":" so, for example,
  #> we have study:sex_attract:menses
  #> new_var1 = study
  #> new_var2 = sex_attract
  #> new_var3 = menses
  #> If we just have a main effect of "study" we get:
  #> new_var1 = study
  #> new_var2 = NA
  #> new_var3 = NA

  #split up the interaction terms (results in a list)
  s <- strsplit(me$variable, ":", fixed = TRUE)
  #get the max length of a list
  k <- max(lengths(s))
  #bring parts into a dataframe
  parts <- t(vapply(s, function(x) { length(x) <- k; x }, character(k)))
  #rename variables by "new_var" and the number
  colnames(parts) <- paste0("new_var", seq_len(k))
  #bind the parts into the original effects data frame
  me <- cbind(me, parts)

  #grab the rows without an interaction term (i.e., all of the main effects) so
  #that I can rename those.
  rename_rows <- fixed_effects[!grepl("\\*", fixed_effects)]

  #Create a new list to save the vector to store the renames
  new_renames <- vector()

  #> loop over the all of the non-interaction terms (i.e., unique variables in model)
  for (i in 1:length(rename_rows)) {
    #grab the current variable name
    var <- as.character(rename_rows[i])
    #create the specific
    new_name <- create_nice_name(data, var)
    new_renames <- c(new_renames, new_name)
  }
  #> This results in the full naming vector where it includes the mapping for all
  #> of the unique variables in the modle (don't need to grab interaction terms
  #> because interaction terms are just made up of main effects)

  #if I don't supply a renaming vector, create a blank one
  if(is.null(rename_vec)) rename_vec <- vector()

  #add the new nice_names vector onto the one provided (or blank one if not provided)
  nice_names <- c(rename_vec, new_renames)


  output <- list(
    model_effects = me,
    nice_names = nice_names
  )
  return(output)

}



#' Rename coefficient rows of a single model using a mapping
#'
#' Creates a tidy coefficient table for one model by merging coefficients with
#' Wald confidence intervals, constructing a variable-name mapping (via
#' \code{create_nice_names()}), applying that mapping to split interaction parts,
#' and finally rebuilding a clean \code{var_star} column such as
#' \code{"study * sex"}.
#'
#' @param model A fitted model object (\code{lm} or \code{lmerMod}).
#' @param data  The data frame used to fit \code{model} (used to infer factor encodings).
#' @param rename_vec Optional pre-existing mapping vector (names = original
#'   variable, values = model-encoded coefficient label) to prepend/extend the
#'   internally created mapping.
#'
#' @return A data frame with a first column \code{var_star} containing the
#'   human-readable term names, followed by the merged coefficient/CI columns.
#'   Interaction split-helper columns are removed in the returned object.
#'
#' @details
#' Internally, the function:
#' \enumerate{
#'   \item Extracts coefficients and CIs (\code{confint()}; Wald for \code{lmerMod}).
#'   \item Calls \code{create_nice_names()} to split interaction terms and build the mapping.
#'   \item Applies the mapping (via \code{rename_rows()}, assumed available) to the split parts.
#'   \item Rejoins split parts into \code{var_star} using \code{" * "} separators.
#' }
#'
#' @examples
#' \dontrun{
#' tbl <- rename_lmer(model = fit, data = df)
#' head(tbl)
#' }
#'
#' @importFrom stats confint
#' @family renaming_helpers
#' @export
rename_lmer <- function(model, data, rename_vec = NULL) {

  #> this function is useful to call in itself, but it only operates on a single
  #> model. "rename_lmers" operates on a list of models.

  #> expanded_model is the full model information that needs to include the following
  #> (1) fixed_effects
  #> (2) model results (i.e., the lmer model)
  #> (3) summary of model results (i.e., ms <- summary(lmer))
  #> rename_vec is the original vector of columns to rename (called "nice_names"
  #> originally)

  #> Things to be mindful of:
  #> This function calls two important functions I've written to help with this...
  #> (1) create_nice_names
  #>   (a) remember, create_nice_names calls create_nice_name within it
  #> (2) rename_rows

  #> This function outputs a fully renamed model_effects table, where all of the
  #> variable names are nice (and are specified by a renaming vector)


  # --- Accept a bare model object (lm / merMod) too ---
  # if (inherits(model, c("lm", "merMod"))) {
  #   mr <- model
  #   ms <- summary(mr)
  #   fixed_effects <- attr(terms(mr), "term.labels")
  #   model <- list(mr = mr,
  #                 ms = ms,
  #                 fixed_effects = fixed_effects)
  # }

  #> Grabbing the specific model information
  mr <- model
  ms <- summary(mr)
  fixed_effects <- grab_fixed_effects(mr, include_intercept = FALSE)
  #> NOTE: The intercept will still be included in output, but I just don't
  #> include the intercept in my renaming vector here.

  ##### GRAB MODEL COEFFICIENTS AND WALD CI AND FORM A TABLE

  #bringing variable names to a column
  model_coef <- as.data.frame(ms$coefficients)
  model_coef$variable <- rownames(model_coef)
  rownames(model_coef) <- NULL


  # ---------- CI: choose method by model class ----------
  if (inherits(mr, "merMod")) {
    model_ci <- as.data.frame(confint(mr, method = "Wald")) #lmer
  }
  if (inherits(mr, "lm")) {
    model_ci <- as.data.frame(confint(mr))  # lm
  }


  model_ci$variable <- rownames(model_ci)

  # merge (automatically drops random effects terms picked up in confint)
  coef_ci <- merge(model_coef, model_ci, by = "variable")
  rownames(coef_ci) <- NULL
  coef_ci$effect <- "fixed"

  #Renaming the coefficient/confidence interval table as "model_effects"
  #> to get plugged into further renaming functions
  model_effects <- coef_ci

  #### END OF CREATING TABLE WITH MODEL EFFECTS AND WALD CI'S
  #> Includes t, df, p, etc.

  #> Here I create the mapping vector which will help me rename the variables
  #> as they were renamed by lmer.
  renaming_info <- create_nice_names(model_effects = model_effects,
                                     fixed_effects = fixed_effects,
                                     rename_vec = rename_vec,
                                     data = data)
  #> model_effects is a table where the interaction terms are still split up into
  # different variables and are not yet renamed
  model_effects <- renaming_info$model_effects
  #> nice_names is the name mapping vector which can be used on model_effects
  # to rename the column
  nice_names <- renaming_info$nice_names

  #rename the rows from the model effects table with split up column names
  #created by "create_nice_names"
  model_effects <- rename_rows(model_effects, nice_names)

  #> combine the updated split up interaction term columns back together

  # pick all var columns dynamically
  var_cols <- grep("^new_var\\d+$", names(model_effects), value = TRUE)

  #create the new "variable" column, which has updated column names
  model_effects$var_star <- apply(model_effects[var_cols], 1, function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x)) paste(x, collapse = " * ") else NA_character_
  })

  #organize so "var_star" column is first and others are afterwards
  model_effects <- model_effects[, c("var_star", setdiff(names(model_effects), "var_star"))]
  #remove split up columns
  model_effects <- model_effects[, !names(model_effects) %in% c(var_cols, "variable")]

  #> return the model effects table with updated variable names
  return(model_effects)
}



#' Rename coefficient rows for a list of models
#'
#' Iterates over \code{model_list}, retrieves each fitted model via \code{model_path},
#' builds a tidy coefficient table with \code{rename_lmer()}, and stores it as
#' \code{$tidy_tbl} on each list element.
#'
#' @param model_list A list whose elements contain (at least) a fitted model object.
#' @param rename_vec  Optional pre-existing mapping vector passed to \code{rename_lmer()}.
#' @param data        The data frame used to fit the models (used to infer factor encodings).
#' @param model_path  Character vector naming the path inside each list element at which
#'   to find the fitted model (e.g., \code{c("mc","final_model")} or \code{c("mr")}).
#'
#' @return The input \code{model_list}, with a new element \code{$tidy_tbl} added
#'   to each processed entry containing the renamed coefficient table.
#'
#' @examples
#' \dontrun{
#' models_expanded <- rename_lmers(models_expanded, rename_vec = NULL,
#'                                 data = df, model_path = c("mc","final_model"))
#' models_expanded[[1]]$tidy_tbl
#' }
#'
#' @seealso \code{\link{rename_lmer}}, \code{\link{create_nice_names}}
#' @family renaming_helpers
#' @export
rename_lmers <- function(model_list, rename_vec, data, model_path) {

  #> NOTE: this function (or rather, the rename_lmer() function) can also handle
  #> renaming the output from regular linear models.

  #model_list = a list of all of my models, which must include:
  #mr = model fit results (mr <- lmer(formula, data))
  #ms = model summary (i.e., ms <- summary(mr))
  #fixed_effects = a character vector of fixed effects

  #> If the model list does not contain those objects, it might be because they
  #> are stored within the "mc" (model comparison) list within the model_list.
  #> from_mlm_comp = TRUE will search each model list to determine if there is
  #> a "mc" component. If there is, it will grab the results from there.

  #This function creates a new element for each model list within the master list
  #> It outputs "tidy_lmer" for each model, which is the renamed table.

  #create a temporary model_list element to work with so I can add
  #tidy_lmer to the original list, but not add other unwanted things to the
  #final output.

  safe_pluck <- function(x, path) {
    Reduce(function(acc, nm) acc[[nm]], path, init = x)
  }

  for (i in seq_along(model_list)) {

    #this grabs the current model list element.
    #> What information will be included in the model list element depends
    #> on what model list I am pulling. At minimum, for final stages, I will
    #> include the model fit element as that serves as a base to grab everything
    #> else I will pretty much need.
    #> In the original model list element I will included all of the individual
    #> aspects of the model (e.g., fixed effects, dv, random effects, etc.)
    current_mls <- model_list[[i]]

    #grabbing the model results, model summary, and fixed effects
    mr <- safe_pluck(current_mls, model_path) #model results
    ms <- summary(mr) #model summary
    fe <- grab_fixed_effects(mr)

    if(is.null(mr)) {
      message("No model results were found for:",
              names(model_list[i]), ";", names(model_list[[i]]$name),
              "The model was skipped and no table was created")
      next }
    #if any variable is null, skip the renaming creation

    tidy_tbl <- rename_lmer(mr,
                            data = data,
                            rename_vec = rename_vec
    )

    #add the tidy table to the model output
    model_list[[i]]$tidy_tbl <- tidy_tbl

  }
  return(model_list)
}



