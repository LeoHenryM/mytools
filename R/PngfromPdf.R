#' PngfromPdf
#'
#' Transform and save a dataframe as a Latex Pdf and a csv copy in anycase. save pdf in the same doc.
#' @param path Path to the folder with pdfs
#' @param resolution Resolution of the pngs
#' @return Nothing - but create transform the pdfs in the folder into a png
#' @examples
#' PngfromPdf("path/to/pdf/folder")
#' @export

PngfromPdf <- function(path, resolution = 300){

  library(pdftools)
  library(magick)

  output_folder <- paste0(path, "/png")
  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

  # Get list of PDF files
  pdf_files <- list.files(path, pattern = "\\.pdf$", full.names = TRUE)

  # Convert each PDF to PNG
  for (pdf in pdf_files) {
    images <- image_read_pdf(pdf, density = 300)  # Read PDF with high resolution
    output_file <- file.path(output_folder, paste0(tools::file_path_sans_ext(basename(pdf)), ".png"))
    image_write(images, output_file, format = "png")
  }

}
