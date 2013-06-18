#' transform Raster* to data frame ready for ggplot2
#' 
#' @param r \code{Raster*} object transform
#' @param na.rm \code{boolean} should NA pixels be removed
#' @param melt \code{boolean} should the raster be melted (default is TRUE)
#' @details \code{melt} is used to facilitate ploting of raster stacks
#' @return a data frame containing \code{x}, \code{y}, the coordinates;  \code{variable}, the layer names; \code{value} the pixel value.
#' @export
as_data = function (r, na.rm = FALSE, melt = TRUE) {
  xy = as.data.frame(r, xy = TRUE)
  if(melt) xy = melt(xy, id = c("x", "y"))
  if (na.rm) xy = na.omit(xy)
  return (xy)
}
