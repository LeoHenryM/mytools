#' #' calculate_minor_breaks: Calculate Minor Axis Breaks
#'
#' This function calculates minor axis breaks based on the main breaks to enhance the readability of plots. The minor breaks are calculated to fall between the major breaks, with the number of minor breaks adjusted to avoid overcrowding the axis.
#' @description Internal function that calculates minor axis breaks. It uses the main axis breaks (calculated using `calculate_breaks`) as a basis and introduces additional minor breaks between them. The function aims to improve plot readability without overcrowding the axis. The number of minor breaks is dynamically adjusted based on the spacing of the major breaks.
#' @param limits Numeric vector of length 2, specifying the lower and upper limits of the axis.
#' @return Numeric vector containing the calculated minor breaks, or NULL if the addition of minor breaks would result in an overcrowded axis.
#' @export


calculate_minor_breaks <- function(limits) {
  breaks <- calculate_breaks(limits)
  if (length(breaks) > 8) {
    return(NULL)
  } else {
    minor_breaks <- seq(min(breaks), max(breaks), by = (min(diff(breaks)) / 2))
    minor_breaks <- minor_breaks[!minor_breaks %in% breaks]
    return(minor_breaks)
  }
}
