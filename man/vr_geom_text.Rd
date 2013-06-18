\name{vr_geom_text}
\alias{vr_geom_text}
\title{Custom visual raster text plot (for raster values and indexes)}
\usage{
  vr_geom_text(r, color = I("#222222"), label = NULL, ...)
}
\arguments{
  \item{r}{\code{Raster*}}

  \item{color}{\code{color} line colors}

  \item{label}{to override default labelling (raster
  values), you can provide a vector of length
  \code{ncell(r)}}

  \item{...}{other arguments passed to
  \code{\link{geom_text}}}
}
\description{
  Custom visual raster text plot (for raster values and
  indexes)
}

