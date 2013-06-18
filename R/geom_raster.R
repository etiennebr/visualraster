#' Custom plot for Visual Raster
#' 
#' Uses discrete values (maximum 11)
#' @param r \code{Raster*}
#' @param color \code{color} line colors
#' @param size \code{integer} line size
#' @param ... other arguments passed to \code{\link{geom_tile}}
#' @export
vr_geom_raster = function(r, color = I("#222222"), size = I(0.2), ...) {
  list(
    geom_tile(data = as_data(r), 
              aes(x = x, y = y, fill = factor(round(value))), 
              color = color, size = size, ...),
    scale_fill_manual("", values = pal$qualitative),
    coord_equal()
  )
}

#' Custom visual raster plot for sequential data
#' 
#' @param r \code{Raster*}
#' @param color \code{color} line colors
#' @param size \code{integer} line size
#' @details if \code{ncell(r) > 5e2} will use geom_raster by default for 
#' performance 
#' @param ... other arguments passed to \code{\link{geom_tile}}
#' @export
vr_geom_raster_seq = function (r, color = I("#222222"), size = I(0.2), ...) {
  if(ncell(r) > 5e2) {
    # use raster if the number of cells is too high
    geo = geom_raster(data = as_data(r), aes(x, y, fill = value)) 
  } else {
    geo = geom_tile(data = as_data(r), 
                    aes(x = x, y = y, fill = value, label = round(value, 1)), 
                    color = color, size = size, ...)
  }
  list(
    geo,
    scale_fill_gradient(low = "#FFFFB3", high = "#FB8072"),
    coord_equal()
  )
}

#' Custom visual raster plot for diverging data
#' 
#' @param r \code{Raster*}
#' @param ... other arguments passed to \code{\link{vr_geom_raster_seq}}
#' @export
vr_geom_raster_div = function (r, ...) {
  list(
    vr_geom_raster_seq(r, ...),
    scale_fill_gradientn(colours = c("#D53E4F", "#FFFFBF", "#3288BD"))
  )
}

#' Custom visual raster text plot (for raster values and indexes)
#' 
#' @param r \code{Raster*}
#' @param color \code{color} line colors
#' @param label to override default labelling (raster values), you can provide 
#' a vector of length \code{ncell(r)}
#' @param ... other arguments passed to \code{\link{geom_text}}
#' @export
vr_geom_text = function (r, color = I("#222222"), label = NULL, ...) {
  dd = as_data(r)
  dd$value = round(dd$value, 1)
  if(is.null(label)) {
    geom_text(data = dd, aes(x = x, y = y, label = value), color = color, ...)
  } else {
    geom_text(data = dd, aes(x = x, y = y), label = label, color = color, ...)
  }
}

#' Custom visual raster point plot
#' 
#' @param p \code{data.frame} containing a \code{x} and \code{y} column
#' @param size \code{integer} point size
#' @param color \code{color} line colors
#' @param fill \code{color} fill colors
#' @param shape shpae of the point (default 21) see \code{\link{geom_point}} 
#' for details
#' @param ... other arguments passed to \code{\link{geom_point}}
#' @export
vr_geom_point = function (p, size = 4, shape = 21, color = "#222222", fill = "#22222280", ...) { 
  geom_point(data = p, aes(x = x, y = y), size = size, color = color, fill = fill, shape = shape, ...)
}

#' Custom visual raster line plot
#' 
#' @param l \code{data.frame} containing a \code{x} and \code{y} column
#' @param size \code{integer} point size
#' @param color \code{color} line colors
#' @param ... other arguments passed to \code{\link{geom_line}}
#' @export
vr_geom_line = function (l, size = 2, color = "#11111199", ...) { 
  geom_line(data = l, aes(x, y), size = size, color = color, ...)
}

#' Custom visual raster polygon plot
#' 
#' @param p \code{data.frame} containing at least \code{x} and \code{y} column 
#' (see \code{\link{fortify}} for a transforming spatial polygons)
#' @param size \code{integer} line size
#' @param color \code{color} border colors
#' @param fill \code{color} fill colors
#' @param ... other arguments passed to \code{\link{geom_polygon}}
#' @export
vr_geom_polygon = function (p, size = 1, fill = "#22222280", color = "#11111199", ...) { 
  geom_polygon(data = p, aes(x=long, y=lat), size = size, fill = fill, color = color, ...)
}
