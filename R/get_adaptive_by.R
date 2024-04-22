#' get_adaptive_by
#'
#' Identify the right by for breaks in a ggplot from the range of value.
#' @description Internal function to calculate adaptive 'by' value based on input range.
#' @param range Numeric, the range for which to calculate 'by'.
#' @return Numeric, the 'by' value.
#' @noRd

get_adaptive_by <- function(range) {
  # Identify the magnitude of the range to determine the appropriate 'by' value
  magnitude <- 10^floor(log10(range))
  leading_digit <- range / magnitude
  if (leading_digit <= 2.5) {
    by <- 0.25
  } else if (leading_digit <= 5) {
    by <- 0.5
  } else {
    by <- 1
  }
  by <- by * magnitude
  return(by)
}
