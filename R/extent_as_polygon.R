#' Transform raster extent to polygon
#' 
#' @param e \code{Raster*} or \code{Extent} object to transform as a foritfied polygon
#' @return \code{data.frame} containing \code{x}, \code{y}, the coordinates; \code{id} the polygon id (always equal to 1)
#' @export
extent_as_poly = function (e) {
  e = extent(e)
  e_poly = data.frame(
    x = c(e@xmin, e@xmax, e@xmax, e@xmin), 
    y = c(e@ymin, e@ymin, e@ymax, e@ymax), 
    id = 1)
  e_poly = rbind(e_poly, e_poly[1, ])
  e_poly
}
