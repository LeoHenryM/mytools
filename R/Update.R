#' Update
#'
#' This function update the package.
#' @noRd

Update <- function() {
  library(devtools)
  devtools::document()
  devtools::install()

}
