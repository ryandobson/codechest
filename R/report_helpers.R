

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



#' Internal p-value formatter
#'
#' Formats p-values with fixed digits and \code{"< 0.001"} threshold.
#' @keywords internal
#' @noRd
.format_p <- function(p, digits = 3) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < .001, "< 0.001", formatC(p, digits = digits, format = "f")))
}


#' Internal significance stars
#'
#' Returns "", ".", "*", "**", or "***" based on p-value.
#' @keywords internal
#' @noRd
.sig_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01,  "**",
                       ifelse(p < .05,  "*",
                              ifelse(p < .1,   ".", "")))))
}

