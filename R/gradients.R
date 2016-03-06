#' Smooth gradient between two colours
#' 
#' Overrides the default continuous gradient as a yellow to red
#' @param low Colour for low end of the gradient
#' @param high Colour for high end of the gradient
#' @param ... Other arguments passed on to \code{\link{scale_fill_continuous}}
#' @export
scale_fill_continuous = function(low = "#FFFFB3", high = "#FB8072", ...) {
  scale_fill_gradient(low = low, high = high, ...)
}

#' Qualitative colour scale
#' 
#' Overrides the default discrete gradient to use color brewer Set 3
#' @param values a set of aesthetic values to map data values to. If this is a 
#' named vector, then the values will be matched based on the names. If unnamed,
#'  values will be matched in order (usually alphabetical) with the limits of 
#'  the scale. Any data values that don't match will be given na.value.
#' @param ... Other arguments passed on to \code{\link{scale_fill_manual}}
#' @import RColorBrewer
#' @export
scale_fill_discrete = function(values = brewer.pal(11, "Set3"), ...) {
  scale_fill_manual(values = values, ...)
}

#' Smooth gradient for diverging data
#' 
#' @param low Colour for low end of the gradient
#' @param high Colour for high end of the gradient
#' @param center Colour for center of the gradient (default very close to white)
#' @param ... Other arguments passed on to \code{\link{scale_fill_gradientn}}
#' @export
scale_fill_diverge = function(low = "#D53E4F", high = "#3288BD", 
                                 center = "#FFFFBF", ...) {
  scale_fill_gradientn(colours = c(low, center, high), ...)  
}
