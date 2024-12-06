

utils::globalVariables(c("nrows", "count"))


#' Generate Histogram for a Given Variable
#'
#' This function generates a histogram for a specified variable in a data frame.
#' It handles both numeric and factor variables, creating appropriate captions
#' and titles for the graph based on the characteristics of the variable.
#'
#' @param df A data frame containing the variable to be plotted.
#' @param variable The variable in the data frame for which the histogram is generated. This can be either a numeric or a factor variable.
#'
#' @return A `ggplot` object representing the histogram.
#' @export
#' @importFrom rlang ensym
#' @importFrom dplyr filter group_by summarize n
#' @importFrom DescTools Mode
#' @importFrom ggplot2 ggplot aes geom_histogram geom_bar coord_cartesian scale_x_continuous scale_y_continuous labs geom_text element_blank element_text element_line element_rect expansion unit guide_axis
#' @importFrom stringr str_wrap
#' @examples
#' # Example of using generate_histogram with a numeric variable
#' dynamic_histogram(ryanhonorthesis, friendship.duration.wks)
#'
#' # Example of using generate_histogram with a factor variable
#' dynamic_histogram(ryanhonorthesis, osf_closer_ssf)
dynamic_histogram <- function(df, variable) {

  #> Introduction  ---------
  #making the variable a symbol to pass it into things outside of the tidyverse selections
  var_sym <- rlang::ensym(variable)

  # Saving the variables named attribute to append to title of graph
  #title <- var_label(df[[var_sym]]) #using the labelled package is unecessary
  title <- attr(df[[var_sym]], "label") #base R version will be more evergreen

  # Calculating the total number of valid (and NA) cases used, to put in caption
  caption1 <- base::paste("N =", sum(!is.na(df[[var_sym]]))) #calculates valid n
  caption2 <- base::paste0(caption1, ";") #adds a semi-colon for clarity in graph
  caption3 <- base::paste(caption2, "NA = ", sum(is.na(df[[var_sym]]))) #calculates number of "na"

  # Remove NA values from the variable (they will be removed automatically anyway)
  df <- df |> dplyr::filter(!is.na(!!var_sym))

#> Conditions --------
  if (is.double(df[[var_sym]]) | is.integer(df[[var_sym]]) && length(unique(df[[var_sym]])) >= 12) { #If the variable is a double (numeric) and there are more than 12 unique values

    #calculate range of x-axis (for caption)
    max_x <- base::max(df[[var_sym]], na.rm = TRUE)
    min_x <- base::min(df[[var_sym]], na.rm = TRUE)

    #Editing the caption to add the range for double variable
    caption4 <- base::paste0(caption3, ";")
    caption5 <- base::paste(caption4, "Range = ", min_x, "to", max_x)

    #Zooming x-coordinates so graph looks better
    coord_max <- stats::quantile(df[[var_sym]], probs = .95, na.rm = TRUE)
    coord_min <- stats::quantile(df[[var_sym]], probs = .05, na.rm = TRUE)


    #Calculating the bin numbers so they are meaningful
    bin_seq <- base::round((coord_max - coord_min) / 12) #12 = number of bins on x-axis
    if (bin_seq == 0) { #experienced an issue where the bin_seq went to 0 with a variable
      bin_seq <- 1 #simplest fix was to just set the bin_seq to 1 if it gets set to 0
    }

    #calculating a good number of bins
    num_bins <- base::ceiling(base::log2(base::length(df[[var_sym]])) + 1)

    modal_n <- DescTools::Mode(df[[var_sym]], na.rm = TRUE) #finding the mode of variable
    freq_mode <- base::attr(modal_n, "freq") #grabbing frequency of the mode

    #Creating some new maxes so the graph looks better
    upp_bound <- base::round((freq_mode * 1.10) / 100) * 100

    #Creating a sequence by number so the graph looks better
    if (freq_mode > 1000) {
      seq_by <- base::round((freq_mode * .10) / 100) * 100 #multiple the frequency by .10 and round it to nearest 100.
    }
    else {
      seq_by <- base::round((freq_mode * .10) / 10) * 10
    }

    histogram <- df |> ggplot2::ggplot(mapping = ggplot2::aes(x = {{variable}})) +
      ggplot2::geom_histogram(
        binwidth = 1,
        bins = num_bins,
        fill = "lightblue"
      ) +
      ggplot2::coord_cartesian(xlim = c(coord_min, coord_max)) +
      ggplot2::scale_x_continuous(
        breaks = seq(coord_min, coord_max, by = bin_seq),
        expand = ggplot2::expansion(mult = c(.025, 0))
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, upp_bound, by = seq_by),
        expand = ggplot2::expansion(mult = c(0, .1))
      ) +
      jermeys_theme() +
      ggplot2::guides(x = ggplot2::guide_axis(angle = 30)) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 24*0.6, color = "black", lineheight = 0.9),
        axis.text.y = ggplot2::element_text(size = 24*0.6, color = "black", lineheight = 0.9),
        plot.title = ggplot2::element_text(size = 24*0.75, color = "black", margin = ggplot2::margin(10, 0, 5, 0))
      ) +
      ggplot2::labs(
        y = "Count",
        title = stringr::str_wrap(title, width = 100),
        caption = caption5
      )

  }

  else if (is.factor(df[[var_sym]])) {

    #calculating the number of levels the variable has
    bin_n <- base::length(base::levels(df[[var_sym]]))
    scale_max <- bin_n - 1


    #calculating what the largest group size is
    max_n <- df |> dplyr::group_by({{variable}}) |>
      dplyr::summarize(nrows = dplyr::n()) |>
      dplyr::summarize(max(nrows)) |>
      base::as.double()

    #Creating some new maxes so the graph looks better
    upp_bound <- base::round((max_n * 1.10) / 100) * 100

    #Creating a sequence by number so the graph looks better
    if (max_n > 1000) {
      seq_by <- base::round((max_n * .10) / 100) * 100
    }
    else {
      seq_by <- base::round((max_n * .10) / 10) * 10
    }

    histogram <- df |> ggplot2::ggplot(mapping = ggplot2::aes(x = {{variable}})) +
      ggplot2::geom_bar(fill = "lightblue") +
      ggplot2::coord_cartesian(xlim = c(1, bin_n)) +
      ggplot2::scale_x_discrete(labels = levels(df[[var_sym]])) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, upp_bound, by = seq_by),
        expand = ggplot2::expansion(mult = c(.0, .15))
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(ggplot2::after_stat(round(count / sum(count) * 100, 2)), "%")), #remove sum(count) * 100 for raw count
        stat = "count",
        vjust = -.6,
        size = 5
      ) +
      jermeys_theme() +
      ggplot2::guides(x = ggplot2::guide_axis(angle = 30)) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 24*0.6, color = "black", lineheight = 0.9),
        axis.text.y = ggplot2::element_text(size = 24*0.6, color = "black", lineheight = 0.9),
        plot.title = ggplot2::element_text(size = 24*0.60, color = "black",margin = ggplot2::margin(10, 0, 5, 0))
      ) +
      ggplot2::labs(
        title = stringr::str_wrap(title, width = 100),
        y = "Count",
        caption = caption3
      )
  }

  else {
    histogram <- NULL
    print(paste(var_sym, "is not a factor or double with range >= 12"))
  }

  #> Output ------
  return(histogram)

}

