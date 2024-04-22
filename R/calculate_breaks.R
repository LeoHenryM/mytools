#' calculate_breaks: Calculate Axis Breaks
#'
#' This function calculates the breaks for an axis based on the limits provided. It uses an adaptive approach to determine suitable intervals between breaks, aiming for a visually appealing and informative axis scale.
#' @description Internal function that calculates axis breaks by determining an appropriate interval ('by') value and applying it within the specified limits. It leverages the `get_adaptive_by` function to adjust the 'by' value based on the range of the data.
#' @param limits Numeric vector of length 2, specifying the lower and upper limits of the axis.
#' @return Numeric vector containing the calculated breaks within the specified limits.
#' @export


calculate_breaks <- function(limits) {
  range <- diff(limits)
  by <- get_adaptive_by(range)
  breaks <- seq(from = ceiling(limits[1] / by) * by, to = floor(limits[2] / by) * by, by = by)
  breaks <- breaks[breaks >= min(limits) & breaks <= max(limits)]
  return(breaks)
}
