#' A minimalist ggplot2 theme for ploting rasters
#'  
#' Adapted from \href{https://github.com/hadley/ggplot2/wiki/Themes}{ggplot2 wiki}
#' @param base_size \code{numeric} base font size
#' @param base_family \code{character} base font family
#' 
#' @return An object of class \code{\link{theme}}.
#' @export
theme_fullframe = function (base_size = 12, base_family = ""){
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.length = grid::unit(0, "lines"),
      axis.ticks.margin = grid::unit(0, "lines"),
      legend.position = "none",
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.margin = grid::unit(0, "lines"),
      plot.background = element_blank(),
      strip.background = element_rect(colour = "#ffffff", fill="#eeeeee")
    )
}

#' A minimalist ggplot2 theme for ploting geographic rasters
#'  
#' Alters \code{\link{theme_fullframe}} by adding x axis elements to indicate
#' longitude. This theme is used for illustrating \code{\link{rotate}}
#' @param base_size \code{numeric} base font size
#' @param base_family \code{character} base font family
#' 
#' @return An object of class \code{\link{theme}}.
#' @export
theme_geographic = function (base_size = 12, base_family = ""){
  theme_fullframe(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text.x = element_text(), 
          axis.ticks = element_line(colour = "#aaaaaa", size = 0.2), 
          axis.ticks.length = grid::unit(1, "mm"),
          axis.ticks.margin = grid::unit(1, "mm")
    ) 
}
