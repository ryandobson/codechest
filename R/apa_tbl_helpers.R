
#' @importFrom flextable flextable add_header_row merge_at align bold italic
#' @importFrom flextable border_remove hline hline_top hline_bottom width
#' @importFrom flextable set_table_properties font fontsize autofit
#' @importFrom flextable add_footer_lines compose as_paragraph as_i merge_at
#' @importFrom flextable save_as_image
#' @importFrom officer fp_border
#' @importFrom stats anova
NULL

#' @title Create an APA-Style Likelihood Ratio Test (Model Comparison) Table
#'
#' @description
#' Formats a model comparison table (e.g., output from [anova()]) into an
#' APA-style table using the **flextable** and **officer** packages.
#' The function supports automatic formatting of numerical columns,
#' bolding of significant model comparisons, and highlighting the best-fitting
#' model based on AIC or BIC.
#'
#' @details
#' This function is intended for use when comparing nested mixed or multilevel
#' models (e.g., from [lme4::lmer()] or [nlme::lme()]) using likelihood ratio tests.
#' It automatically:
#' \itemize{
#'   \item extracts key columns from the ANOVA comparison table,
#'   \item reformats numeric and p-value columns to APA standards,
#'   \item adds hierarchical bold/italic headers (title and subtitle),
#'   \item bolds rows corresponding to significant likelihood ratio tests,
#'   \item optionally bolds the best-fitting model (lowest AIC or BIC),
#'   \item and returns a fully formatted **flextable** object ready for
#'   export to Word or PDF.
#' }
#'
#' @param anova_table A data frame, typically created by [anova()] on a list of
#'   nested models. Must contain model fit indices (e.g., AIC, BIC, logLik, Chisq, Df, Pr(>Chisq)).
#' @param bold_title A character string specifying the main table title (bolded).
#'   Defaults to `"Table"`.
#' @param italics_title A character string specifying the secondary subtitle
#'   (italicized). Defaults to `"Model Comparison (Likelihood Ratio Test)"`.
#' @param table_note Optional character string for a note appended to the footer.
#' @param font_size Numeric. Font size applied to all table elements.
#'   Defaults to `10`.
#' @param font Character string specifying the font name (e.g., `"Times New Roman"`).
#' @param bold_best Logical. Whether to bold the best-fitting model
#'   (smallest AIC or BIC). Defaults to `TRUE`.
#' @param sig_level Logical or numeric. If `FALSE`, no significance bolding is
#'   applied. If numeric (e.g., `.05`), rows with p-values below this level are
#'   bolded to indicate significant likelihood ratio tests. Defaults to `FALSE`.
#'
#' @return A flextable object (see [flextable::flextable()] for table export options).
#'
#' @examples
#' \dontrun{
#' # Example with two nested lmer models
#' library(lme4)
#' m1 <- lmer(Reaction ~ 1 + (1 | Subject), sleepstudy)
#' m2 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
#' anova_tbl <- anova(m1, m2)
#' apa_anova_comparison(anova_tbl, bold_title = "Table 1", italics_title = "Model Comparison")
#' }
#'
#' @seealso
#' [flextable::flextable()], [anova()], [lme4::lmer()]
#'
#' @export
apa_anova_comparison <- function(anova_table,
                                 bold_title = "Table",
                                 italics_title = "Model Comparison (Likelihood Ratio Test)",
                                 table_note = "",
                                 font_size = 10,
                                 font = "Times New Roman",
                                 bold_best = TRUE,
                                 sig_level = FALSE) {
  if (!inherits(anova_table, "data.frame"))
    stop("Input must be a data.frame (e.g., from anova()).")

  # ---- 1. Prepare columns ----
  df_out <- anova_table

  # Add model names from rownames
  df_out$Model <- rownames(df_out)
  rownames(df_out) <- NULL

  # Select and rename columns (handle variations gracefully)
  needed_cols <- c("Model", "npar", "AIC", "BIC", "logLik", "-2*log(L)",
                   "Chisq", "Df", "Pr(>Chisq)")
  existing_cols <- intersect(needed_cols, names(df_out))
  df_out <- df_out[, existing_cols, drop = FALSE]

  # Rename for nice header
  colnames(df_out)[colnames(df_out) == "-2*log(L)"] <- "-2LL"
  colnames(df_out)[colnames(df_out) == "Pr(>Chisq)"] <- "p"

  # ---- 2. Format numeric columns ----
  num_fmt <- function(x, digits = 0) ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), x))

  df_out$AIC <- num_fmt(df_out$AIC)
  df_out$BIC <- num_fmt(df_out$BIC)
  df_out$logLik <- num_fmt(df_out$logLik)
  if ("-2LL" %in% names(df_out)) df_out$`-2LL` <- num_fmt(df_out$`-2LL`)
  if ("Chisq" %in% names(df_out)) df_out$Chisq <- ifelse(is.na(df_out$Chisq), "", sprintf("%.1f", df_out$Chisq))
  if ("Df" %in% names(df_out)) df_out$Df <- ifelse(is.na(df_out$Df), "", as.character(df_out$Df))

  # p-values
  if ("p" %in% names(df_out)) {
    pvals <- df_out$p
    df_out$p <- ifelse(is.na(pvals), "",
                       ifelse(pvals < .001, "< .001",
                              sub("^0\\.", ".", sprintf("%.3f", pvals))))
  }

  # ---- 3. Reorder columns (if needed) ----
  col_order <- c("Model", "npar", "AIC", "BIC", "logLik", "-2LL", "Chisq", "Df", "p")
  df_out <- df_out[, intersect(col_order, names(df_out)), drop = FALSE]

  # ---- 4. Build flextable ----
  ft <- flextable::flextable(df_out)

  # ---- 5. Clean header text ----
  clean_text <- function(x) {
    x <- gsub("(?<!\n)\n(?!\n)", " ", x, perl = TRUE)
    x <- gsub(" {2,}", " ", x)
    trimws(x)
  }
  bold_title_clean <- clean_text(bold_title)
  italics_title_clean <- clean_text(italics_title)
  table_note_clean <- clean_text(table_note)

  # ---- 6. Add APA headers ----
  ft <- flextable::add_header_row(ft, values = italics_title_clean, colwidths = rep(ncol(df_out), 1))
  ft <- flextable::add_header_row(ft, values = bold_title_clean, colwidths = rep(ncol(df_out), 1))
  ft <- flextable::merge_h(ft, i = 1:2, part = "header")
  ft <- flextable::align(ft, i = 1:2, align = "left", part = "header")
  ft <- flextable::bold(ft, i = 1, part = "header", bold = TRUE)
  ft <- flextable::italic(ft, i = 2, part = "header", italic = TRUE)

  # ---- 7. Align and style ----
  ft <- flextable::align(ft, j = "Model", align = "left", part = "body")
  ft <- flextable::align(ft, j = setdiff(names(df_out), "Model"), align = "right", part = "body")
  ft <- flextable::align(ft, i = 3, part = "header", align = "center")
  ft <- flextable::italic(ft, part = "header", i = 3, italic = TRUE)

  # Borders
  ft <- flextable::border_remove(ft)
  std_border <- officer::fp_border(color = "black", width = 1)
  ft <- flextable::hline(ft, i = 2, border = std_border, part = "header")
  ft <- flextable::hline(ft, i = 3, border = std_border, part = "header")
  ft <- flextable::hline_bottom(ft, border = std_border, part = "body")

  # ---- 8. Bold significant model comparisons ----

  # (a) Bold significant model comparisons
  if ("Pr(>Chisq)" %in% names(anova_table)) {
    p_raw <- anova_table[["Pr(>Chisq)"]]
    # make sure it's numeric (sometimes prints like "< 2.2e-16" but is numeric under the hood)
    sig_rows <- which(!is.na(p_raw) & p_raw < sig_level)
    if (length(sig_rows) > 0) {
      # body rows are 1:nrow(df_out), so no +1 here
      ft <- flextable::bold(ft, i = sig_rows, part = "body", bold = TRUE)
    }
  }

  # (b) Bold best-fitting model based on AIC (smaller = better)

  if (bold_best) {
    best_row <- NA
    if ("AIC" %in% names(anova_table)) {
      aic_vals <- suppressWarnings(as.numeric(anova_table$AIC))
      if (any(!is.na(aic_vals))) best_row <- which.min(aic_vals)
    } else if ("BIC" %in% names(anova_table)) {
      bic_vals <- suppressWarnings(as.numeric(anova_table$BIC))
      if (any(!is.na(bic_vals))) best_row <- which.min(bic_vals)
    }

    if (!is.na(best_row)) {
      ft <- flextable::bold(ft, i = best_row, part = "body", bold = TRUE)
    }
  }

  # ---- 9. Footer note ----
  if (nzchar(table_note_clean)) {
    ft <- flextable::add_footer_lines(ft, values = rep("", ncol(df_out)))
    ft <- flextable::compose(
      ft, i = 1, j = 1, part = "footer",
      value = flextable::as_paragraph(
        flextable::as_i("Note. "),
        table_note_clean
      )
    )
    ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df_out), part = "footer")
    ft <- flextable::align(ft, align = "left", part = "footer")
  }

  # ---- 10. Font and size ----
  ft <- flextable::set_table_properties(ft, layout = "autofit", width = .76)
  ft <- flextable::font(ft, fontname = font, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")

  ft
}






#' @title Create an APA-Style Summary Table for Multilevel Model Comparisons
#'
#' @description
#' Generates a publication-ready **flextable** summarizing the stepwise
#' random-effects model comparison process (e.g., random-slope removal,
#' covariance testing). Designed to provide concise APA-style reporting
#' of the model-building sequence produced by [mlm_comparison()] and
#' organized via [make_mlm_report()].
#'
#' The resulting table displays:
#' \itemize{
#'   \item A **bold title** (e.g., "Random Effects Decision Process: EP_PRCPSMSy").
#'   \item An **italic subtitle** (e.g., "Mixed Model Random-Effects Comparison").
#'   \item A single **Description** column listing each model comparison step
#'         in plain text (e.g., "Comparison 2: Add covariances | kept: m2").
#'   \item Three horizontal lines: one below the subtitle, one below the
#'         column header ("Description"), and one at the table's bottom.
#' }
#'
#' The function can operate in either `"short"` or `"long"` mode:
#' \describe{
#'   \item{`"short"`}{Produces only the summary table described above.}
#'   \item{`"long"`}{Includes the summary table plus one APA-formatted
#'   likelihood-ratio comparison table per model step, created via
#'   [apa_anova_comparison()].}
#' }
#'
#' @details
#' Titles and subtitles are cleaned using an internal `clean_text()` helper
#' to trim excess whitespace and newlines. Borders and alignment are formatted
#' to match APA style conventions:
#' \itemize{
#'   \item Bold title and italic subtitle are left-aligned with no lines above.
#'   \item Plain header ("Description") separates the title block and table body.
#'   \item Lines are drawn only (1) below the subtitle, (2) below the header,
#'         and (3) below the final row.
#' }
#'
#' @param mlm_report An object of class `"mlm_report"`, typically created by
#'   [make_mlm_report()] after running [mlm_comparison()].
#' @param bold_title Character string specifying the top (bold) title.
#'   Defaults to `"Table"`.
#' @param italics_title Character string specifying the italic subtitle.
#'   Defaults to `"Mixed Model Random-Effects Comparison"`.
#' @param font Character string specifying the font name applied to all text.
#'   Defaults to `"Times New Roman"`.
#' @param font_size Numeric value for font size applied to all text.
#'   Defaults to `11`.
#' @param sig_level Numeric significance threshold passed to
#'   [apa_anova_comparison()] for bolding p-values in sub-tables.
#'   Defaults to `0.05`.
#'
#' @return
#' Depending on the verbosity level in `mlm_report`:
#' \describe{
#'   \item{`"short"`}{Returns a single **flextable** object containing the
#'   APA-style summary of random-effects decisions.}
#'   \item{`"long"`}{Returns a list of **flextable** objects with:
#'     \itemize{
#'       \item `$summary` – the summary table of model comparison steps, and
#'       \item `$comparison_X` – one or more sub-tables from
#'         [apa_anova_comparison()] for each model comparison.
#'     }}
#' }
#'
#' @examples
#' \dontrun{
#' library(lme4)
#'
#' # Run stepwise model comparison
#' hist_obj <- mlm_comparison(
#'   data = sleepstudy,
#'   dv = "Reaction",
#'   fixed_effects = "Days",
#'   random_effects = c("1", "Days"),
#'   group = "Subject"
#' )
#'
#' # Generate report structure
#' report <- make_mlm_report(hist_obj)
#'
#' # Create APA-style table (short summary only)
#' apa_tbl <- apa_mlm_report(report, verbosity = "short")
#' apa_tbl
#' }
#'
#' @seealso
#' [make_mlm_report()], [mlm_comparison()], [apa_anova_comparison()],
#' [flextable::flextable()], [officer::fp_border()]
#'
#' @export
apa_mlm_report <- function(mlm_report,
                           bold_title = "Table",
                           italics_title = "Mixed Model Random-Effects Comparison",
                           font = "Times New Roman",
                           font_size = 11,
                           sig_level = 0.05) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' is required for apa_mlm_report().", call. = FALSE)
  }

  verbosity <- mlm_report$verbosity %||% "long"

  # --- helper functions ---
  clean_text <- function(x) {
    # trim leading/trailing whitespace and condense multiple spaces/newlines
    x <- gsub("\\s+", " ", x)
    x <- gsub("\n+", "\n", x)
    trimws(x)
  }

  .final_is_correlated <- function(steps, idx) {
    if (idx <= 1) return(NA)
    prev <- steps[[idx - 1L]]
    if (!identical(prev$type, "add_covariances")) return(NA)
    if (identical(prev$kept, "m2")) TRUE
    else if (identical(prev$kept, "m1")) FALSE
    else NA
  }

  .show_vec <- function(v, max_terms = 6) {
    if (!length(v)) return("(none)")
    if (length(v) > max_terms)
      paste0(paste(v[seq_len(max_terms)], collapse = ", "), ", ... (", length(v), " terms)")
    else paste(v, collapse = ", ")
  }

  ## ---------------- SHORT VERSION (summary only) ---------------- ##
  make_summary_table <- function() {
    step_texts <- character()
    for (i in seq_along(mlm_report$steps)) {
      st <- mlm_report$steps[[i]]
      label <- switch(st$type,
                      initial_model = sprintf("Initial model: %s (uncorrelated)", .show_vec(st$random_from)),
                      drop_smallest = {
                        comp_num <- sum(vapply(
                          mlm_report$steps[seq_len(i)],
                          function(s) s$type %in% c("drop_smallest", "add_covariances", "ri_versus_fixed"),
                          logical(1)
                        ))
                        sprintf("Comparison %d: Remove random slope | kept: %s", comp_num, st$kept)
                      },
                      add_covariances = {
                        comp_num <- sum(vapply(
                          mlm_report$steps[seq_len(i)],
                          function(s) s$type %in% c("drop_smallest", "add_covariances", "ri_versus_fixed"),
                          logical(1)
                        ))
                        sprintf("Comparison %d: Add covariances | kept: %s", comp_num, st$kept)
                      },
                      ri_versus_fixed = {
                        comp_num <- sum(vapply(
                          mlm_report$steps[seq_len(i)],
                          function(s) s$type %in% c("drop_smallest", "add_covariances", "ri_versus_fixed"),
                          logical(1)
                        ))
                        sprintf("Comparison %d: Random intercept vs fixed effects | kept: %s", comp_num, st$kept)
                      },
                      final_model = {
                        is_corr <- .final_is_correlated(mlm_report$steps, i)
                        corr_txt <- if (isTRUE(is_corr)) "(correlated)"
                        else if (identical(is_corr, FALSE)) "(uncorrelated)"
                        else ""
                        sprintf("Final model: %s %s", .show_vec(st$random_to), corr_txt)
                      },
                      NA_character_
      )
      step_texts <- c(step_texts, label)
    }

    df_summary <- data.frame(
      Description = step_texts,
      stringsAsFactors = FALSE
    )

    bold_title_clean <- clean_text(bold_title)
    italics_title_clean <- clean_text(italics_title)

    ft_summary <- flextable::flextable(df_summary)

    # --- Build APA-style header block ---


    ft_summary <- flextable::add_header_row(
      ft_summary,
      values = italics_title_clean,
      colwidths = ncol(df_summary)
    )

    ft_summary <- flextable::add_header_row(
      ft_summary,
      values = bold_title_clean,
      colwidths = ncol(df_summary)
    )

    ft_summary <- flextable::bold(ft_summary, i = 1, part = "header", bold = TRUE)
    ft_summary <- flextable::align(ft_summary, i = 1, part = "header", align = "left")


    ft_summary <- flextable::italic(ft_summary, i = 2, part = "header", italic = TRUE)
    ft_summary <- flextable::align(ft_summary, i = 2, part = "header", align = "left")


    # --- Remove all default borders first ---
    ft_summary <- flextable::border_remove(ft_summary)

    # define a thin visible border line
    thin_line <- officer::fp_border(width = 1, color = "black")

    # --- Add the three desired lines ---
    # --- Add the three desired lines ---
    ft_summary <- flextable::hline(ft_summary, i = 2, border = thin_line, part = "header")   # below italics title
    ft_summary <- flextable::hline_top(ft_summary, border = thin_line, part = "body")        # below "Description"
    ft_summary <- flextable::hline_bottom(ft_summary, border = thin_line, part = "body")     # bottom of table


    # --- Formatting ---
    ft_summary <- flextable::fontsize(ft_summary, size = font_size)
    ft_summary <- flextable::font(ft_summary, fontname = font)
    ft_summary <- flextable::autofit(ft_summary)
    ft_summary
  }



  # --- Short mode: only the summary table
  if (verbosity == "short") {
    ft_summary <- make_summary_table()
    class(ft_summary) <- c("apa_mlm_report", class(ft_summary))
    return(ft_summary)
  }

  ## ---------------- LONG VERSION (summary + ANOVA comparisons) ---------------- ##
  ft_list <- list()
  ft_list$summary <- make_summary_table()
  comp_idx <- 0

  for (i in seq_along(mlm_report$steps)) {
    st <- mlm_report$steps[[i]]
    if (st$type %in% c("drop_smallest", "add_covariances", "ri_versus_fixed")) {
      comp_idx <- comp_idx + 1

      corr1 <- if (identical(st$type, "add_covariances")) "(uncorrelated)" else ""
      corr2 <- if (identical(st$type, "add_covariances")) "(correlated)" else ""
      model1_str <- paste(.show_vec(st$random_from), corr1)
      model2_str <- paste(.show_vec(st$random_to), corr2)

      if (!is.null(st$mod_comp)) {
        ft_anova <- apa_anova_comparison(
          anova_table = st$mod_comp,
          bold_title = sprintf("Random Effects Comparison %d", comp_idx),
          italics_title = sprintf(
            "Likelihood Ratio Test of Random Effects: Model 1 structure: %s; Model 2 structure: %s",
            model1_str, model2_str
          ),
          font = font,
          font_size = font_size,
          sig_level = sig_level
        )
        ft_list[[paste0("comparison_", comp_idx)]] <- ft_anova
      }
    }
  }

  class(ft_list) <- c("apa_mlm_report", class(ft_list))
  ft_list
}



#' @title Generate APA-Style Summary Tables for Multiple MLM Comparisons
#'
#' @description
#' Iterates through a list of model objects containing multilevel model (MLM)
#' comparison histories and produces APA-formatted summary tables for each.
#' Each table is generated using [apa_mlm_report()], displaying the model
#' comparison process (e.g., random-slope removal, covariance tests) in
#' a concise, publication-ready format.
#'
#' @details
#' The function is typically used after running [mlm_comparison()] and
#' constructing an `"mlm_report"` object with [make_mlm_report()].
#' It automatically extracts the comparison history (e.g., `$mc`) from each
#' element in `model_list`, builds a structured report with [make_mlm_report()],
#' and formats the output via [apa_mlm_report()].
#'
#' Titles for each model are generated dynamically:
#' \itemize{
#'   \item The **bold_title** appears as the top row (commonly left blank or set
#'         to `"Table"` for numbered tables).
#'   \item The **italics_title** is constructed as
#'         `paste0(title_prefix, model_name)` (e.g.,
#'         *"Mixed Model Random-Effects Comparison: EP_PRCPSMSy"*).
#' }
#'
#' The resulting APA-style tables are stored back into each model list element
#' under `$apa_mlm_report`. These outputs can later be saved as images or DOCX
#' files using [save_apa_mlm_reports()].
#'
#' @param model_list A named list of model specifications, each containing a
#'   comparison history object (typically `$mc`) created by [mlm_comparison()].
#' @param mc_path Character string specifying the nested path to the MLM
#'   comparison object within each list element (e.g., `"mc"` or `"results$mc"`).
#'   Defaults to `"mc"`.
#' @param bold_title Character string specifying the bold header line for all
#'   APA tables (e.g., `"Table"`). Defaults to an empty string.
#' @param title_prefix Character string prepended to each model name to create
#'   the italicized subtitle (e.g.,
#'   `"Mixed Model Random-Effects Comparison: "`). Defaults to that phrase.
#' @param font Character string specifying the font family to use throughout
#'   the tables. Defaults to `"Times New Roman"`.
#' @param font_size Numeric font size applied to all text. Defaults to `11`.
#' @param sig_level Numeric significance threshold passed to
#'   [apa_anova_comparison()] for bolding p-values. Defaults to `0.2`.
#' @param verbosity Character string indicating the level of detail in the
#'   reports. Must be `"long"` (summary + detailed ANOVA tables) or `"short"`
#'   (summary table only). Defaults to `"long"`.
#'
#' @return
#' A list identical to the input `model_list`, but with an added element
#' `$apa_mlm_report` in each model. Each `$apa_mlm_report` contains one or more
#' **flextable** objects formatted in APA style:
#' \describe{
#'   \item{`$summary`}{APA-style summary of the random-effects decision process.}
#'   \item{`$comparison_X`}{One or more detailed likelihood-ratio test tables
#'         (if `verbosity = "long"`).}
#' }
#'
#' @examples
#' \dontrun{
#' # Suppose each element in `models` contains an `$mc` object
#' # generated by mlm_comparison().
#'
#' # Generate APA-formatted summaries for all models
#' models <- run_apa_mlm_report(
#'   model_list = models,
#'   mc_path = "mc",
#'   bold_title = "Table",
#'   title_prefix = "Random Effects Decision Process: ",
#'   font = "Garamond",
#'   font_size = 12,
#'   sig_level = .20,
#'   verbosity = "long"
#' )
#'
#' # Access the formatted flextable for the first model
#' models[[1]]$apa_mlm_report$summary
#' }
#'
#' @seealso
#' [apa_mlm_report()], [make_mlm_report()], [mlm_comparison()],
#' [save_apa_mlm_reports()], [flextable::flextable()]
#'
#' @export
run_apa_mlm_report <- function(model_list,
                               mc_path = "mc",             # <- path to the mlm_comparison() output
                               bold_title = "",
                               title_prefix = "Mixed Model Random-Effects Comparison: ",
                               font = "Times New Roman",
                               font_size = 11,
                               sig_level = .2,
                               verbosity = c("long", "short")) {

  verbosity <- match.arg(verbosity)

  # simple safe accessor (handles nested paths like "results$mc")
  safe_pluck <- function(x, path) {
    parts <- strsplit(path, "\\$")[[1]]
    Reduce(function(acc, nm) acc[[nm]], parts, init = x)
  }

  for (i in seq_along(model_list)) {

    model_name <- names(model_list)[i]
    title_i <- paste0(title_prefix, model_name)

    # grab mlm_comparison history object dynamically
    mc_obj <- safe_pluck(model_list[[i]], mc_path)

    if (is.null(mc_obj)) {
      message(sprintf("Skipping %s: no '%s' object found.", model_name, mc_path))
      next
    }

    # make textual report
    report_i <- make_mlm_report(
      history = mc_obj,
      title = title_i,
      verbosity = verbosity
    )

    # make APA flextable(s)
    apa_report_i <- apa_mlm_report(
      mlm_report = report_i,
      bold_title = bold_title,
      italics_title = title_i,
      font = font,
      font_size = font_size,
      sig_level = sig_level
    )

    model_list[[i]]$apa_mlm_report <- apa_report_i
  }

  return(model_list)
}

#' @title Save APA-Style MLM Reports to Image Files
#'
#' @description
#' Exports APA-style mixed-model report tables (created by [apa_mlm_report()]
#' or [run_apa_mlm_report()]) as `.png` image files.
#' Each table (summary or comparison) is saved to a specified output directory,
#' optionally within an automatically created subfolder.
#'
#' @details
#' This function loops through a list of model specifications that each contain
#' an APA-formatted report (stored under the element specified by `report_path`).
#' For each model, all **flextable** objects are converted to high-resolution
#' `.png` images using [flextable::save_as_image()].
#'
#' By default, images are saved to a subdirectory named after the prefix
#' (e.g., `"apa_mlm_report"`). You can disable subfolder creation or provide
#' a custom folder name using the `create_subfolder` argument.
#'
#' @param model_list A named list of model specifications, each containing
#'   an APA-style report object (typically `$apa_mlm_report`).
#' @param directory Character string specifying the base directory where
#'   images will be saved. Created recursively if it does not exist.
#' @param report_path Character string indicating the path to the APA report
#'   within each model object (e.g., `"apa_mlm_report"`). Defaults to `"apa_mlm_report"`.
#' @param prefix Character string used as the filename prefix for all saved
#'   image files and the default subfolder name. Defaults to `"apa_mlm_report"`.
#' @param create_subfolder Logical or character.
#'   - If `TRUE`, a subfolder named after `prefix` is created.
#'   - If `FALSE`, images are saved directly into `directory`.
#'   - If a character string is provided, it is used as a custom subfolder name.
#'
#' @return
#' Invisibly returns the input `model_list` after saving all report tables
#' as `.png` images.
#' Files are written to disk; a message indicates the final output location.
#'
#' @examples
#' \dontrun{
#' # Generate APA reports
#' models <- run_apa_mlm_report(model_list)
#'
#' # Save all reports as PNGs in a new subfolder
#' save_apa_mlm_reports(models, directory = "output/reports")
#' }
#'
#' @seealso
#' [apa_mlm_report()], [run_apa_mlm_report()], [flextable::save_as_image()]
#'
#' @importFrom flextable save_as_image
#' @export
save_apa_mlm_reports <- function(model_list,
                                 directory,
                                 report_path = "apa_mlm_report",
                                 prefix = "apa_mlm_report",
                                 create_subfolder = TRUE) {
  # ---- Handle folder creation ----
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)

  # Allow user to specify whether to create a new subfolder or use the base directory
  if (isTRUE(create_subfolder)) {
    directory <- file.path(directory, prefix)
  } else if (is.character(create_subfolder)) {
    # user provided custom folder name as string
    directory <- file.path(directory, create_subfolder)
  }

  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)

  # ---- Loop over models ----
  for (i in seq_along(model_list)) {
    model_name <- names(model_list)[i]
    report <- model_list[[i]][[report_path]]

    if (is.null(report)) {
      message(sprintf("Skipping %s: no APA MLM report found.", model_name))
      next
    }

    # --- Short version: single flextable ---
    if (inherits(report, "flextable")) {
      file_name <- file.path(directory, paste0(prefix, "_", model_name, "_summary.png"))
      flextable::save_as_image(report, path = file_name)
      next
    }

    # --- Long version: list of flextables (summary + comparisons) ---
    for (j in seq_along(report)) {
      sub_name <- names(report)[j]
      if (inherits(report[[j]], "flextable")) {
        file_name <- file.path(directory,
                               paste0(prefix, "_", model_name, "_", sub_name, ".png"))
        flextable::save_as_image(report[[j]], path = file_name)
      }
    }
  }

  message(sprintf("Saved all APA MLM report tables to: %s", normalizePath(directory)))
}


#' Create APA-Style Summary Table for lmer Models
#'
#' Generates a fully formatted APA-style table of fixed-effect estimates from a
#' linear mixed-effects model (`lmerMod` or `lmerModLmerTest`). The table is
#' output as a [`flextable`](https://davidgohel.github.io/flextable/) object,
#' ready for inclusion in Word, Quarto, or PDF documents. It optionally adds
#' model diagnostics (ICC, R², sample size) in the table footer and supports
#' custom variable labels, predictor ordering, and selective bolding of
#' significant or specified effects.
#'
#' @param model A fitted `lmerMod` or `lmerModLmerTest` object.
#' @param data The data frame used to fit the model (required for renaming and
#'   internal consistency checks).
#' @param nice_names Optional named vector for renaming predictors to more
#'   readable labels.
#' @param bold_title Character string giving the bolded title line (e.g.,
#'   `"Table"`).
#' @param italics_title Character string giving the italicized subtitle
#'   (e.g., `"Fixed Effects"`).
#' @param table_note Character string appended as a note at the bottom of the
#'   table. Additional notes (ICC, R², etc.) are added automatically when
#'   `extra_note_info = TRUE`.
#' @param font_size Numeric scalar; font size (points) for the table text.
#'   Default is `10`.
#' @param font Character scalar giving the font family to use. Default is
#'   `"Times New Roman"`.
#' @param effects_to_bold Optional character vector of predictor names or
#'   interaction terms to bold manually (e.g., `c("X1", "X1*X2")`).
#' @param sig_level Numeric scalar giving the significance threshold below
#'   which effects are automatically bolded. Default is `.05`.
#' @param extra_note_info Logical; if `TRUE` (default), appends ICC, R²,
#'   and sample size information using
#'   [`performance::icc()`][performance::icc] and
#'   [`performance::r2()`][performance::r2].
#' @param reorder_predictors Logical; if `TRUE` (default), sorts predictors by
#'   interaction order (main effects first, then two-way, then higher-order).
#'
#' @details
#' Internally, the function:
#' \enumerate{
#'   \item Extracts model coefficients using a helper (`rename_lmer()`).
#'   \item Formats estimates, *t*-values, *p*-values, and confidence intervals
#'     to APA precision.
#'   \item Creates a [`flextable::flextable()`] object with hierarchical
#'     header rows (bold and italic titles).
#'   \item Optionally bolds statistically significant predictors and/or user-specified
#'     effects.
#'   \item Optionally appends notes for number of observations, number of groups,
#'     Intraclass Correlation Coefficients (adjusted / unadjusted), and
#'     model R² (conditional / marginal) computed via the
#'     [`performance`](https://easystats.github.io/performance/) package.
#' }
#'
#' @return A formatted [`flextable`] object representing an APA-style model
#' summary table.
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' library(performance)
#' m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' apa_lmer_model(m, data = sleepstudy,
#'                bold_title = "Table 1",
#'                italics_title = "Fixed Effects",
#'                table_note = "All coefficients are unstandardized.",
#'                extra_note_info = TRUE)
#' }
#'
#' @seealso [performance::icc()], [performance::r2()],
#'   [flextable::flextable()], [lme4::lmer]
#'
#' @importFrom flextable flextable add_header_row merge_at align bold italic
#'   border_remove hline hline_bottom autofit set_table_properties
#'   font fontsize add_footer_lines compose as_paragraph as_i width
#' @importFrom officer fp_border
#' @importFrom performance icc r2
#' @importFrom lme4 getME ngrps
#' @importFrom stats nobs
#' @importFrom utils capture.output
#' @export
apa_lmer_model <- function(model, data,
                           nice_names = NULL,
                           bold_title = "Table",
                           italics_title = "",
                           table_note = "",
                           font_size = 10,
                           font = "Times New Roman",
                           effects_to_bold = NULL,
                           sig_level = .05,
                           extra_note_info = TRUE,
                           reorder_predictors = TRUE) {

  clean_text <- function(x) {
    x <- gsub("(?<!\n)\n(?!\n)", " ", x, perl = TRUE)
    x <- gsub(" {2,}", " ", x)
    trimws(x)
  }

   # ---- 1. Extract and rename columns ----
  df_out <- rename_lmer(model, data, rename_vec = nice_names)
  df_out <- df_out[, c("var_star", "Estimate", "Std. Error", "df",
                       "t value", "Pr(>|t|)", "2.5 %", "97.5 %")]
  names(df_out) <- c("Predictor", "Estimate", "SE", "df", "t", "p", "conf.low", "conf.high")

  # Keep numeric p for later significance check
  p_numeric <- df_out$p

  # ---- 2. Format before table ----
  df_out$Estimate <- sprintf("%.2f", df_out$Estimate)
  df_out$t <- sprintf("%.2f", df_out$t)
  df_out$df <- sprintf("%.2f", df_out$df)
  df_out$CI <- sprintf("[%.2f, %.2f]", df_out$conf.low, df_out$conf.high)

  df_out$p <- ifelse(p_numeric < .001, "< .001", sprintf("%.3f", p_numeric))
  df_out$p <- sub("^0\\.", ".", df_out$p)  # remove leading zeros

  # Keep only desired columns
  df_out <- df_out[, c("Predictor", "Estimate", "CI", "t", "df", "p")]

  # ---- 2b. Reorder predictors: main -> 2-way -> 3-way ----
  if (reorder_predictors) {
    # count "*" per predictor (0 = main effect, 1 = two-way, 2+ = higher)
    m <- gregexpr("\\*", df_out$Predictor, perl = TRUE)
    star_counts <- lengths(regmatches(df_out$Predictor, m))  # 0 for no matches

    ord <- order(star_counts)
    df_out <- df_out[ord, ]
    p_numeric <- p_numeric[ord]  # keep alignment for bolding significant rows
  }

  # ---- 3. Build flextable ----
  ft <- flextable::flextable(df_out)

  #> Ensuring nice printing of names:
  # Step 1: replace single newlines (not double) with a space
  italics_title_clean <- clean_text(italics_title)

  # Add table title rows
  ft <- flextable::add_header_row(
    ft,
    values = italics_title_clean,
    colwidths =  rep(ncol(df_out), 1)
  )

  # Step 1: replace single newlines (not double) with a space
  bold_title_clean <- cleant_text(bold_title)

  ft <- flextable::add_header_row(
    ft,
    values = bold_title_clean,
    colwidths =  rep(ncol(df_out), 1)
  )
  # Merge each header row across all columns
  ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df_out), part = "header")
  ft <- flextable::merge_at(ft, i = 2, j = 1:ncol(df_out), part = "header")

  # APA style: left-align both
  ft <- flextable::align(ft, i = 1:2, j = 1, part = "header", align = "left")

  #Bold Table Header
  ft <- flextable::bold(ft, i = 1, part = "header", bold = TRUE)
  #Make Title italics
  ft <- flextable::italic(ft, i = 2, part = "header", italic = TRUE)


  # ---- 4. Styling header columns of what values are ----
  ft <- flextable::align(ft, j = "Predictor", align = "left", part = "body")
  ft <- flextable::align(ft, j = c("Estimate", "CI", "t", "df", "p"), align = "right", part = "body")
  ft <- flextable::align(ft, i = 3, align = "center", part = "header")
  ft <- flextable::italic(ft, i = 3, part = "header", italic = TRUE)

  # Horizontal rules
  ft <- flextable::border_remove(ft)
  std_border <- officer::fp_border(color = "black", width = 1)
  ft <- flextable::hline(ft, i = 2, border = std_border, part = "header")
  ft <- flextable::hline(ft, i = 3, border = std_border, part = "header")
  ft <- flextable::hline_bottom(ft, border = std_border, part = "body")

  # Bold significant rows
  sig_rows <- which(p_numeric < sig_level)
  if (length(sig_rows) > 0) {
    ft <- flextable::bold(ft, i = sig_rows, part = "body", bold = TRUE)
  }


  # ---- 4b. Bold specific effects manually ----
  if (!is.null(effects_to_bold) && length(effects_to_bold) > 0) {

    effects_to_bold <- as.character(effects_to_bold)

    # Helper function to split a term into clean component names
    split_terms <- function(x) {
      strsplit(x, "\\s*[:\\*xX]\\s*")[[1]] |> trimws()
    }

    # Prepare a normalized version of Predictors in the table
    pred_components <- lapply(df_out$Predictor, split_terms)

    custom_rows <- integer(0)

    for (term in effects_to_bold) {
      term_parts <- split_terms(term)

      # Match predictors with the same set of components (order-insensitive)
      matches <- sapply(pred_components, function(p) setequal(p, term_parts))
      custom_rows <- c(custom_rows, which(matches))
    }

    custom_rows <- unique(custom_rows)

    if (length(custom_rows) == 0) {
      message("No matches found for 'effects_to_bold' in Predictor column.")
    } else {
      ft <- flextable::bold(ft, i = custom_rows, part = "body", bold = TRUE)
    }
  }


  ft <- flextable::autofit(ft)

  # ---- 5. Add table note ----


  table_note <- clean_text(table_note)

  if (extra_note_info == TRUE) {

    # --- ICCs ---
    model_icc <- performance::icc(model)

    # --- R2 values ---
    model_r2 <- performance::r2(model)
    r2_cond <- round(model_r2$R2_conditional, 2)
    r2_marg <- round(model_r2$R2_marginal, 2)

    # --- Observation and group info ---
    n <- lme4::getME(model, "N")
    ng <- lme4::ngrps(model)

    obs_note <- paste0("Number of Observations: ", n)
    gr_note  <- paste0("Number of Groups: ", ng)
    obs_gr_note <- paste0(obs_note, "; ", gr_note)

    # --- ICC section ---
    icc_adj_note   <- paste0("Adjusted ICC: ", round(model_icc$ICC_adjusted, 2))
    icc_unadj_note <- paste0("Unadjusted ICC: ", round(model_icc$ICC_unadjusted, 2))
    icc_notes <- paste0(icc_adj_note, "; ", icc_unadj_note)

    # --- R² section ---
    r2_cond_note <- paste0("Conditional R2: ", r2_cond)
    r2_marg_note <- paste0("Marginal R2: ", r2_marg)
    r2_notes <- paste0(r2_cond_note, "; ", r2_marg_note)

    # --- Reference note ---
    icc_calc_note <- paste0(
      "ICC and R2 calculated using icc() and r2() from the performance package; ",
      "see package documentation and Nakagawa et al. (2017) for calculation details."
    )

    # --- Combine all notes ---
    table_note <- paste0(
      table_note, "\n",
      obs_gr_note, "\n",
      icc_notes, "\n",
      r2_notes, "\n",
      icc_calc_note
    )
  }

  ft <- flextable::add_footer_lines(ft, values = rep("", ncol(df_out)))
  ft <- flextable::compose(
    ft, i = 1, j = 1, part = "footer",
    value = flextable::as_paragraph(
      flextable::as_i("Note. "),
      table_note
    )
  )

  ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df_out), part = "footer")
  ft <- flextable::align(ft, align = "left", part = "footer")
  ft <- flextable::fontsize(ft, part = "footer", size = font_size)

  # ---- 6. Font & size ----
  ft <- flextable::set_table_properties(ft, layout = "autofit")
  ft <- flextable::font(ft, fontname = font, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")

  # (optional) Limit the total table width if needed (e.g., 6.5 inches for APA margins)
  ft <- flextable::set_table_properties(ft,
                                        layout = "autofit",
                                        width = .76) #proportion of page width.
  #.76 x 8.5 = 6.46

  return(ft)
}


#' @title Generate APA-Style Tables for Multiple Linear Mixed-Effects Models
#'
#' @description
#' Iterates over a list of model specifications, extracts the final fitted model
#' from each (via [final_model_fed()]), and produces APA-style **flextable**
#' outputs using [apa_lmer_model()].
#' Each formatted table is added back into the model list under the element
#' `$apa_table`.
#'
#' @details
#' This function is a convenience wrapper for batch-generating publication-ready
#' tables of fixed effects from multiple `lmerTest::summary()` models.
#' It automatically handles numbering of table titles, retrieval of model data,
#' and consistent formatting options (font, note, and significance bolding).
#'
#' The user may specify a global data frame (`data`) shared across models or,
#' alternatively, a data path within each model list entry (`data_path`).
#' If a global data frame is not provided, the function attempts to locate the
#' dataset by evaluating the object name specified at `data_path`.
#'
#' @param model_list A named list of model specifications, each containing a
#'   model comparison object (`$mc_menses`) from which the final model can be
#'   extracted via [final_model_fed()].
#' @param data Optional data frame used for all models. If `NULL`, each model’s
#'   dataset is retrieved via `data_path`.
#' @param data_path Character string specifying the nested path to the data
#'   element within each model entry (e.g., `"data"`). Defaults to `"data"`.
#' @param nice_names Optional named vector mapping original variable names to
#'   user-friendly labels (passed to [apa_lmer_model()]).
#' @param model_path Character string specifying the name of the element
#'   within each model object that contains the APA flextable report.
#'   Defaults to `"NULL"`.
#' @param bold_title Character string specifying the base title (bolded in the table).
#'   A numeric index is appended automatically. Defaults to `"Table"`.
#' @param italics_title Character string specifying the italicized subtitle.
#'   Typically includes the model name or formula.
#' @param table_note Optional character string for a note displayed at the table footer.
#' @param font_size Numeric font size applied to all text. Defaults to `10`.
#' @param font Character string specifying the font family (e.g., `"Times New Roman"`).
#' @param effects_to_bold Optional character vector of predictor names or
#'   interaction terms to bold manually in all tables.
#' @param sig_level Numeric significance threshold for automatic bolding of
#'   significant effects. Defaults to `0.05`.
#' @param reorder_predictors Logical; if `TRUE` (default), predictors are ordered
#'   by interaction complexity (main effects first, then interactions).
#' @param extra_note_info Logical; if `TRUE` (default), appends ICC, R²,
#'   and sample size information using
#'
#' @return
#' A list identical in structure to `model_list`, but with an additional
#' element `$apa_table` appended to each model entry.
#' Each `$apa_table` is a formatted **flextable** returned by [apa_lmer_model()].
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' # Example model list (simplified)
#' model_list <- list(
#'   Model1 = list(name = "Reaction ~ Days", data = "sleepstudy", mc_menses = mc_obj1),
#'   Model2 = list(name = "Reaction ~ Days + (1|Subject)", data = "sleepstudy", mc_menses = mc_obj2)
#' )
#'
#' # Generate APA-formatted tables for each model
#' results <- run_apa_lmer_model(model_list)
#'
#' # Access the first formatted table
#' results$Model1$apa_table
#' }
#'
#' @seealso
#' [apa_lmer_model()], [final_model_fed()], [flextable::flextable()]
#'
#' @export
run_apa_lmer_model <- function(model_list,
                               data = NULL,
                               data_path = "data",
                               nice_names = NULL,
                               model_path = NULL,
                               bold_title = "Table",
                               italics_title = "",
                               table_note = "",
                               font_size = 10,
                               font = "Times New Roman",
                               effects_to_bold = NULL,
                               sig_level = .05,
                               extra_note_info = TRUE,
                               reorder_predictors = TRUE
) {

  safe_pluck <- function(x, path) {
    Reduce(function(acc, nm) acc[[nm]], path, init = x)
  }

  if(is.null(model_path)) {
   stop("The path where the model is stored in the model list must be
                  supplied")
  }


  for (i in seq_along(model_list)) {


    italics_title_i <- model_list[[i]]$name

    #> If the bold title contains a period (e.g., "Table 3.")
    #> then paste the current number without a space, otherwise, paste the current
    #> number with a space
    if(grepl("\\.", bold_title)) {
      bold_title_i <- paste0(bold_title, i)} else {
        bold_title_i <- paste(bold_title, i)
      }


    # grab mlm_comparison history object dynamically
    model_i <- safe_pluck(model_list[[i]], model_path)

    if (is.null(data)) { #if I don't supply a df that is used across models,
      #then use the path that must be specified
      data <- safe_pluck(model_list[[i]], data_path)
      data <- get(data, envir = parent.frame())
    }

    table_i <- apa_lmer_model(
      model = model_i,
      data = data,
      nice_names = nice_names,
      bold_title = bold_title_i,
      italics_title = italics_title_i,
      table_note = table_note,
      font = font,
      font_size = font_size,
      effects_to_bold = effects_to_bold,
      sig_level = sig_level,
      extra_note_info = extra_note_info,
      reorder_predictors = reorder_predictors
    )

    model_list[[i]]$apa_table <- table_i

  }

  return(model_list)

}



#' @title Save APA-Style LMER Tables to Image Files
#'
#' @description
#' Exports APA-formatted **flextable** objects (created by [apa_lmer_model()]
#' or [run_apa_lmer_model()]) as `.png` image files.
#' Each model's table is saved to a user-specified output directory, with
#' optional automatic subfolder creation and file naming based on a prefix.
#'
#' @details
#' This function loops through a list of model specifications that each contain
#' an APA-style table (by default stored under the element name `"apa_table"`).
#' Each table is converted to a high-resolution image using
#' [flextable::save_as_image()], and written to disk with filenames that include
#' both the specified `prefix` and model name.
#'
#' By default, images are saved to a new subfolder named after the `prefix`
#' (e.g., `"apa_table/"`). Users can disable subfolder creation or provide a
#' custom folder name via the `create_subfolder` argument.
#'
#' @param model_list A named list of model objects, each containing an
#'   APA-formatted **flextable** (e.g., `$apa_table`).
#' @param directory Character string specifying the base directory where
#'   image files will be saved. Created recursively if it does not exist.
#' @param apa_table_path Character string indicating the element name or nested
#'   path to the APA table within each model list element.
#'   Defaults to `"apa_table"`.
#' @param prefix Character string used as both the filename prefix and the
#'   default subfolder name. Defaults to `"apa_table"`.
#' @param create_subfolder Logical or character.
#'   - If `TRUE` (default), a subfolder named after `prefix` is created.
#'   - If `FALSE`, tables are saved directly in `directory`.
#'   - If a character string is provided, it is used as a custom subfolder name.
#'
#' @return
#' Invisibly returns the input `model_list` after saving all tables to disk.
#' A message is printed indicating the final output location.
#'
#' @examples
#' \dontrun{
#' # Generate APA-style tables first
#' models <- run_apa_lmer_model(model_list)
#'
#' # Save tables as .png images in a subfolder
#' save_apa_lmer_tables(models, directory = "output/results")
#'
#' # Save tables directly to the specified folder (no subfolder)
#' save_apa_lmer_tables(models, directory = "output/results", create_subfolder = FALSE)
#' }
#'
#' @seealso
#' [apa_lmer_model()], [run_apa_lmer_model()], [flextable::save_as_image()]
#'
#' @importFrom flextable save_as_image
#' @export

save_apa_lmer_tables <- function(model_list,
                                 directory,
                                 apa_table_path = "apa_table",
                                 prefix = "apa_table",
                                 create_subfolder = TRUE) {

  safe_pluck <- function(x, path) {
    parts <- strsplit(path, "\\$")[[1]]
    for (nm in parts) {
      if (is.null(x)) break
      x <- x[[nm]]
    }
    x
  }

  # ---- Handle folder creation ----
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)

  # Optional subfolder behavior
  if (isTRUE(create_subfolder)) {
    directory <- file.path(directory, prefix)
  } else if (is.character(create_subfolder)) {
    # user provided a custom folder name
    directory <- file.path(directory, create_subfolder)
  }

  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)

  # ---- Loop through model list ----
  for (i in seq_along(model_list)) {
    model_name <- names(model_list)[i]
    table_i <- safe_pluck(model_list[[i]], apa_table_path)

    # Skip if no table found
    if (is.null(table_i)) {
      message(sprintf("Skipping %s: no APA table found at '%s'.",
                      model_name, apa_table_path))
      next
    }

    # Construct filename
    file_name <- file.path(directory,
                           paste0(prefix, "_", model_name, ".png"))

    # Save table image
    flextable::save_as_image(table_i, path = file_name)
  }

  message(sprintf("Saved APA tables to: %s", normalizePath(directory)))
  invisible(model_list)
}




#' Create an APA-style summary table for fixed-effect reduction
#'
#' Generates a formatted \code{flextable} summarizing the iterative process of
#' fixed-effect reduction from a mixed-effects model, as tracked by a
#' \code{"fixed_effect_drop_history"} object (produced by
#' \code{\link{fixed_effect_drops}}). The table concisely displays the
#' random-effect structures at each stage and the fixed effects removed at
#' each iteration, suitable for inclusion in reports, manuscripts, or
#' supplemental materials.
#'
#' @param history An object of class \code{"fixed_effect_drop_history"} created
#'   by \code{\link{fixed_effect_drops}} or added to a model list via
#'   \code{\link{run_fixed_effect_drops}}.
#' @param bold_title Character scalar giving the main (bold) title of the table.
#'   Defaults to \code{"Table"}.
#' @param italics_title Character scalar giving the secondary (italicized) title.
#'   Defaults to \code{"Fixed-Effect Reduction Process"}.
#' @param table_note Optional character string providing a note placed below the
#'   table. The final model fitting method (e.g., REML or ML) is automatically
#'   appended.
#' @param font_size Numeric value specifying the base font size for all table
#'   text. Defaults to \code{10}.
#' @param font Character scalar specifying the font family to use. Defaults to
#'   \code{"Times New Roman"}.
#'
#' @details
#' This function internally calls \code{\link{print.fixed_effect_drop_history}} with
#' \code{verbose = FALSE} to extract key model-reduction details while
#' suppressing console output. It then constructs a two-column APA-style
#' \code{flextable} showing:
#' \itemize{
#'   \item The random-effects structure in the initial, intermediate, and final models.
#'   \item Each fixed-effect term removed at successive steps.
#'   \item A note summarizing the final model fitting method.
#' }
#'
#' @return A formatted \code{flextable} object containing:
#' \describe{
#'   \item{Description}{A narrative label describing each step.}
#'   \item{Details}{The corresponding random or fixed-effect terms.}
#' }
#'
#' @examples
#' \dontrun{
#' fed_hist <- fixed_effect_drops(
#'   model             = fit,
#'   fixed_effects     = fes,
#'   remove            = "menses",
#'   new_random_effect = "menses",
#'   alpha             = 0.10,
#'   data              = df
#' )
#'
#' apa_fed_report(fed_hist)
#' }
#'
#' @seealso
#' \code{\link{fixed_effect_drops}},
#' \code{\link{run_fixed_effect_drops}},
#' \code{\link{print.fixed_effect_drop_history}},
#' \code{\link[flextable]{flextable}}
#'
#' @export

apa_fed_report <- function(history,
                           bold_title = "Table",
                           italics_title = "Fixed-Effect Reduction Process",
                           table_note = "",
                           font_size = 10,
                           font = "Times New Roman") {

  # Validate input class
  stopifnot(inherits(history, "fixed_effect_drop_history"))

  # Capture the returned list from print.fixed_effect_drop_history() without printing to console
  pm <- suppressMessages({
    tmp <- capture.output(res <- print.fixed_effect_drop_history(history, verbose = FALSE))
    res
  })
  # --- Random effects ---
  re_init  <- pm$random_effects$initial$string
  re_final <- pm$random_effects$final$string

  mid_names <- setdiff(names(pm$random_effects$by_step),
                       c("s00_initial_model", "s02_final_model"))
  re_mid <- if (length(mid_names)) {
    pm$random_effects$by_step[[mid_names[1]]]$string
  } else "(none)"

  # --- Fixed effects removed ---
  removed_by_step <- pm$fixed_effects_removed_by_step
  nsteps <- length(removed_by_step)

  label_steps <- function(i) {
    c("First removal", "Second removal", "Third removal")[i] %||% paste("Step", i)
  }

  descs <- character(nsteps)
  details <- character(nsteps)

  for (i in seq_len(nsteps)) {
    descs[i] <- label_steps(i)
    rmv <- removed_by_step[[i]]
    if (length(rmv) == 0) rmv <- "(none)"
    details[i] <- paste(rmv, collapse = ", ")
  }

  # Replace final "(none)" with narrative sentence
  if (details[length(details)] == "(none)") {
    details[length(details)] <- "No new effects identified as non-significant after refitting."
  }

  # --- Combine all rows ---
  df <- data.frame(
    Description = c(
      "Random effects in initial model",
      "Random-effect structure after addition",
      descs,
      "Final model random-effect structure"
    ),
    Details = c(
      re_init,
      re_mid,
      details,
      re_final
    ),
    stringsAsFactors = FALSE
  )

  # --- Clean text ---
  clean_text <- function(x) {
    x <- gsub("(?<!\n)\n(?!\n)", " ", x, perl = TRUE)
    x <- gsub(" {2,}", " ", x)
    trimws(x)
  }

  bold_title_clean <- clean_text(bold_title)
  italics_title_clean <- clean_text(italics_title)
  table_note_clean <- clean_text(table_note)

  # --- Build flextable ---
  ft <- flextable::flextable(df)

  # --- Add APA-style stacked headers ---
  ft <- flextable::add_header_row(ft, values = italics_title_clean,
                                  colwidths = rep(ncol(df), 1))
  ft <- flextable::add_header_row(ft, values = bold_title_clean,
                                  colwidths = rep(ncol(df), 1))
  ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df), part = "header")
  ft <- flextable::merge_at(ft, i = 2, j = 1:ncol(df), part = "header")
  ft <- flextable::align(ft, i = 1:2, j = 1, part = "header", align = "left")
  ft <- flextable::bold(ft, i = 1, part = "header", bold = TRUE)
  ft <- flextable::italic(ft, i = 2, part = "header", italic = TRUE)

  # --- Align and style body ---
  ft <- flextable::align(ft, j = "Description", align = "left", part = "body")
  ft <- flextable::align(ft, j = "Details", align = "left", part = "body")
  ft <- flextable::align(ft, i = 3, part = "header", align = "center")
  #ft <- flextable::italic(ft, i = 3, part = "header", italic = TRUE)

  ft <- flextable::border_remove(ft)
  std_border <- officer::fp_border(color = "black", width = 1)
  ft <- flextable::hline(ft, i = 2, border = std_border, part = "header")
  ft <- flextable::hline(ft, i = 3, border = std_border, part = "header")
  ft <- flextable::hline_bottom(ft, border = std_border, part = "body")

  # --- Add footer note with final fit method ---
  footer_note <- paste0("Final model fit method: ", pm$final_method)
  if (nzchar(table_note_clean))
    footer_note <- paste(table_note_clean, footer_note, sep = "  ")

  ft <- flextable::add_footer_lines(ft, values = rep("", ncol(df)))
  ft <- flextable::compose(
    ft, i = 1, j = 1, part = "footer",
    value = flextable::as_paragraph(
      flextable::as_i("Note. "),
      footer_note
    )
  )
  ft <- flextable::merge_at(ft, i = 1, j = 1:ncol(df), part = "footer")
  ft <- flextable::align(ft, align = "left", part = "footer")
  ft <- flextable::fontsize(ft, part = "footer", size = font_size)

  # --- Font, layout, and fit ---
  # --- Adjust column widths ---
  # --- Adjust column widths ---
  ft <- flextable::width(ft, j = "Description", width = 2)
  ft <- flextable::width(ft,j = "Details", width = 4)

  # Optional: slightly wider overall layout
  ft <- flextable::set_table_properties(ft, layout = "autofit", width = 0.9)
  ft <- flextable::font(ft, fontname = font, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")

  ft
}

#' Generate and Attach APA-Style Fixed-Effect Reduction Reports
#'
#' This function iterates over a list of model objects (e.g., multilevel models)
#' and generates an APA-formatted summary of the fixed-effect reduction process
#' for each model using [apa_fed_report()]. The resulting flextable reports are
#' stored within each model entry in the list for later export or display.
#'
#' @param model_list A named list of model objects, where each element may contain
#'   a fixed-effect reduction history object (e.g., produced by
#'   [fixed_effect_drops()] or `mlm_comparison()`).
#' @param fed_path Character string specifying the nested path (using `$` notation)
#'   to the fixed-effect reduction history object within each model element.
#'   Defaults to `"fed"`.
#' @param title_prefix Character string prefix applied to the title of each
#'   generated report. Defaults to `"Fixed-Effect Reduction Process: "`.
#' @param font Character string specifying the font family for the flextable.
#'   Defaults to `"Times New Roman"`.
#' @param font_size Numeric value indicating the font size used in the report.
#'   Defaults to `11`.
#'
#' @details
#' The function extracts each model’s fixed-effect reduction history using
#' a safe accessor that supports nested list paths (e.g., `"results$fed"`),
#' generates the APA-style table using [apa_fed_report()], and attaches the
#' resulting flextable as a new element (`$apa_fed_report`) to each model
#' entry in the list.
#'
#' @return
#' A list of models identical to the input `model_list` but with an additional
#' `$apa_fed_report` element appended to each model that successfully generated
#' a table.
#'
#' @examples
#' \dontrun{
#' models <- run_apa_fed_report(model_list = my_models, fed_path = "fed")
#' models$Model1$apa_fed_report
#' }
#'
#' @seealso [apa_fed_report()], [save_apa_fed_reports()]
#' @export
run_apa_fed_report <- function(model_list,
                               fed_path = "fed",    # <- path to the fixed_effect_drops() output
                               title_prefix = "Fixed-Effect Reduction Process: ",
                               font = "Times New Roman",
                               font_size = 11) {

  # simple safe accessor (handles nested paths like "results$mc")
  safe_pluck <- function(x, path) {
    parts <- strsplit(path, "\\$")[[1]]
    Reduce(function(acc, nm) acc[[nm]], parts, init = x)
  }

  for (i in seq_along(model_list)) {

    model_name <- names(model_list)[i]
    title_i <- paste0(title_prefix, model_name)

    # grab mlm_comparison history object dynamically
    fed_obj <- safe_pluck(model_list[[i]], fed_path)

    if (is.null(fed_obj)) {
      message(sprintf("Skipping %s: no '%s' object found.", model_name, fed_path))
      next
    }

    # make APA flextable(s)
    apa_report_i <- apa_fed_report(
      history = fed_obj,
      bold_title = "",
      italics_title = title_i,
      table_note = "",
      font = font,
      font_size = font_size
    )

    model_list[[i]]$apa_fed_report <- apa_report_i
  }

  return(model_list)
}

#' Save APA-Style Fixed-Effect Reduction Reports to Disk
#'
#' Saves APA-formatted fixed-effect reduction tables (generated by
#' [run_apa_fed_report()]) as image files in a specified directory.
#' Each table is saved as a `.png` file, optionally organized into
#' a subfolder for convenience.
#'
#' @param model_list A list of model objects containing APA flextables,
#'   typically produced by [run_apa_fed_report()]. Each model should
#'   include a report object at the location specified by `report_path`.
#' @param directory Character string specifying the directory in which
#'   to save the report images. If the directory does not exist, it will
#'   be created recursively.
#' @param report_path Character string specifying the name of the element
#'   within each model object that contains the APA flextable report.
#'   Defaults to `"apa_fed_report"`.
#' @param prefix Character string used as the filename prefix for saved
#'   reports and, optionally, the subfolder name. Defaults to `"apa_fed_report"`.
#' @param create_subfolder Logical or character value controlling subfolder
#'   creation:
#'   * If `TRUE` (default), a subfolder named after `prefix` will be created.
#'   * If a character string is provided, it will be used as the subfolder name.
#'   * If `FALSE`, reports are saved directly to `directory`.
#'
#' @details
#' The function loops through all models in `model_list` and saves any valid
#' flextable object (class `"flextable"`) to disk using
#' [flextable::save_as_image()]. Models without valid APA reports are skipped.
#'
#' @return
#' Invisibly returns `NULL`. The function is primarily called for its side effect
#' of saving image files to disk.
#'
#' @examples
#' \dontrun{
#' save_apa_fed_reports(
#'   model_list = my_models,
#'   directory = "output/reports",
#'   prefix = "fixed_effect_reductions"
#' )
#' }
#'
#' @seealso [run_apa_fed_report()], [flextable::save_as_image()]
#' @export
save_apa_fed_reports <- function(model_list,
                                 directory,
                                 report_path = "apa_fed_report",
                                 prefix = "apa_fed_report",
                                 create_subfolder = TRUE) {
  # ---- Handle folder creation ----
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)

  # Allow user to specify whether to create a new subfolder or use the base directory
  if (isTRUE(create_subfolder)) {
    directory <- file.path(directory, prefix)
  } else if (is.character(create_subfolder)) {
    # user provided custom folder name as string
    directory <- file.path(directory, create_subfolder)
  }

  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)

  # ---- Loop over models ----
  for (i in seq_along(model_list)) {
    model_name <- names(model_list)[i]
    report <- model_list[[i]][[report_path]]

    if (is.null(report)) {
      message(sprintf("Skipping %s: no APA FED report found.", model_name))
      next
    }

    # --- Single flextable ---
    if (inherits(report, "flextable")) {
      file_name <- file.path(directory, paste0(prefix, "_", model_name, ".png"))
      flextable::save_as_image(report, path = file_name)
      next
    }

  }

  message(sprintf("Saved all APA FED report tables to: %s", normalizePath(directory)))
}
