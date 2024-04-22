#' Custom ggplot2 Breaks
#'
#' Adjusts the breaks for ggplot2 axis scales, with specific behavior based on the axis type.
#' For y-axes, it dynamically calculates breaks and minor breaks, optionally formatting the labels as percentages.
#'
#' @param x Character, indicating if the x-axis should be treated as "continuous".
#' @param y Character, indicating additional options for the y-axis, with "nothing" as default,
#'          allowing for automatic calculation of breaks and minor breaks, or formatted labels.
#'
#' @return A list of ggplot2 theme and scale adjustments.
#'
#' @examples
#' ggbreaks(x="continuous", y="nothing")
#' @export

ggbreaks <- function(x="continuous", y="nothing"){
  k <- list(theme(axis.text.x = element_text(angle = 45, hjust = 1)))

  if(x=="continuous"){
    k <- c(k,
           scale_x_continuous(breaks = calculate_breaks,
                              minor_breaks = calculate_minor_breaks))
  }

  if(y=="nothing"){
    k <- c(k,
           scale_y_continuous(breaks = calculate_breaks,
                              minor_breaks = calculate_minor_breaks))
  }else{
    k <- c(k,
           scale_y_continuous(breaks = calculate_breaks,
                              minor_breaks = calculate_minor_breaks,
                              labels = scales::percent))
  }

  return(k)

}
