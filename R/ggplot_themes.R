

utils::globalVariables(c("%+replace%", "element_blank", "element_line",
                         "element_rect", "element_text", "margin", "theme",
                         "theme_grey", "unit"))


#' Custom ggplot2 Theme: Jeremy's Theme
#'
#' A custom theme inspired by Jeremy Hogeveen for ggplot2 visualizations.
#'
#' @param base_size Base font size. Default is 24.
#' @param base_family Base font family. Default is an empty string.
#' @return A ggplot2 theme object.
#' @importFrom ggplot2 %+replace% theme_grey theme element_blank element_text element_line element_rect margin unit
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   jermeys_theme()
#' }
#' @export
jermeys_theme <- function(base_size = 24, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size * 0.8, color = "black", lineheight = 0.9),
      axis.text.y = element_text(size = base_size * 0.8, color = "black", lineheight = 0.9),
      axis.ticks = element_line(color = "black", size = 0.2),
      axis.title.x = element_text(size = base_size, color = "black", margin = margin(10, 0, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "black", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      legend.background = element_rect(color = NA, fill = "#ffffff"),
      legend.key = element_rect(color = "black", fill = "#ffffff"),
      legend.key.size = unit(2, "lines"),
      legend.text = element_text(size = base_size * 0.8, color = "black"),
      legend.title = element_text(size = base_size * 0.8, face = "bold", color = "black"),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_rect(fill = NA, color = "black"),
      panel.grid.major = element_line(color = "#ffffff"),
      panel.grid.minor = element_line(color = "#ffffff"),
      panel.spacing = unit(2, "lines"),
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size * 0.8, color = "black"),
      strip.text.y = element_text(size = base_size * 0.8, color = "black", angle = -90),
      plot.background = element_rect(color = "#ffffff", fill = "#ffffff"),
      plot.title = element_text(size = base_size * 1.2, color = "black"),
      plot.margin = unit(rep(1, 4), "lines")
    )
}



#' Custom ggplot2 Theme: Black Theme
#'
#' A ggplot2 theme with a black background and white text.
#'
#' @param base_size Base font size. Default is 24.
#' @param base_family Base font family. Default is an empty string.
#' @return A ggplot2 theme object.
#' @importFrom ggplot2 %+replace% theme_grey theme element_blank element_text element_line element_rect margin unit
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_black()
#' }
#' @export
theme_black <- function(base_size = 24, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size * 0.8, color = "white", lineheight = 0.9),
      axis.text.y = element_text(size = base_size * 0.8, color = "white", lineheight = 0.9),
      axis.ticks = element_line(color = "white", size = 0.2),
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(10, 0, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      legend.background = element_rect(color = NA, fill = "black"),
      legend.key = element_rect(color = "white", fill = "black"),
      legend.key.size = unit(2, "lines"),
      legend.text = element_text(size = base_size * 0.8, color = "white"),
      legend.title = element_text(size = base_size * 0.8, face = "bold", color = "white"),
      panel.background = element_rect(fill = "black", color = NA),
      panel.border = element_rect(fill = NA, color = "white"),
      panel.grid.major = element_line(color = "grey35"),
      panel.grid.minor = element_line(color = "grey20"),
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size * 0.8, color = "white"),
      strip.text.y = element_text(size = base_size * 0.8, color = "white", angle = -90),
      plot.background = element_rect(color = "black", fill = "black"),
      plot.title = element_text(size = base_size * 1.2, color = "white"),
      plot.margin = unit(rep(1, 4), "lines")
    )
}


#' Custom ggplot2 Theme: Gridline Theme
#'
#' A ggplot2 theme that emphasizes grid lines.
#'
#' @param base_size Base font size. Default is 24.
#' @param base_family Base font family. Default is an empty string.
#' @return A ggplot2 theme object.
#' @importFrom ggplot2 %+replace% theme_grey theme element_blank element_text element_line element_rect margin unit
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   gline_theme()
#' }
#' @export
gline_theme <- function(base_size = 24, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size * 0.8, color = "black", lineheight = 0.9),
      axis.text.y = element_text(size = base_size * 0.8, color = "black", lineheight = 0.9),
      axis.ticks = element_line(color = "black", size = 0.2),
      axis.title.x = element_text(size = base_size, color = "black", margin = margin(10, 0, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "black", angle = 90, margin = margin(0, 10, 0, 0)),
      panel.grid.major = element_line(color = "grey", size = 0.1),
      panel.grid.minor = element_line(color = "#ffffff"),
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = element_text(size = base_size * 0.8, color = "black"),
      strip.text.y = element_text(size = base_size * 0.8, color = "black", angle = -90),
      plot.background = element_rect(color = "#ffffff", fill = "#ffffff"),
      plot.title = element_text(size = base_size * 1.2, color = "black"),
      plot.margin = unit(rep(1, 4), "lines")
    )
}

