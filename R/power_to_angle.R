#' Transform flow directions powers of 2 to angular data
#' 
#' Tranditional flowdir directions are expressed in power of two where 1 is East,
#' 2 is South-East, 4 is South, 8 is South-West, etc.
#' @param x \code{numeric} should be a vector containing powers of 2
#' @param radians \code{boolean} should the result be in radians (default is degrees)
#' @seealso \link{terrain}
#' @export
power_to_angle = function(x, radians = FALSE) {
  modulo = ifelse(radians, 2 * pi, 360)
  (log(x, 2) / 8 * modulo + modulo / 4) %% modulo
}
