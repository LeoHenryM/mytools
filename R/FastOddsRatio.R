#' FastOddsRatio
#'
#' Calculate odds ration from two probability x and y doing the ration of odds(x) on odds(y)
#' @param x A numeric probability.
#' @param y A numeric probability.
#' @return the odds ratio.
#' @examples
#' FastOddsRatio(0.3, 0.1)
#' @export

FastOddsRatio <- function(x, y){
  Ox <- x/(1-x)
  Oy <- y/(1-y)
  OR <- Ox/Oy
  return(OR)
}
