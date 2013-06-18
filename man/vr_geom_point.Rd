\name{vr_geom_point}
\alias{vr_geom_point}
\title{Custom visual raster point plot}
\usage{
  vr_geom_point(p, size = 4, shape = 21, color = "#222222",
    fill = "#22222280", ...)
}
\arguments{
  \item{p}{\code{data.frame} containing a \code{x} and
  \code{y} column}

  \item{size}{\code{integer} point size}

  \item{color}{\code{color} line colors}

  \item{fill}{\code{color} fill colors}

  \item{shape}{shpae of the point (default 21) see
  \code{\link{geom_point}} for details}

  \item{...}{other arguments passed to
  \code{\link{geom_point}}}
}
\description{
  Custom visual raster point plot
}

