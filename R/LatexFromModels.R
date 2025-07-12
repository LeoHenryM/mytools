#' LatexFromModels
#'
#' Transform and save a model - either linear or mixed lmer regression - as a Latex Pdf and a rdata copy in anycase. save pdf in the same doc.
#' @param models A model or a list of modelt
#' @param path The path where to save the new pdf.
#' @param modelnames The names of the models
#' @param title The title of the table
#' @param digits The number of digits to show
#' @return Nothing - but create a pdf doc with the table in the right path
#' @examples
#' LatexFromModels(models, "./output/modeleffect1")
#' @export

LatexFromModels <- function(models, path, modelnames = NULL, title = NULL, digits = 2) {
  save(models, file = paste0(path, ".rdata"))

  if (length(models) != length(modelnames) & !is.null(modelnames)) stop("The length of modelnames does not match the number of models in models")

  latex_code <- texreg::texreg(models,
                               format = "latex",
                               custom.model.names = modelnames,
                               caption = title,
                               caption.above = TRUE,
                               digits = digits)

  path2 <- paste0(path, ".tex")

  height <- max(1, str_count(latex_code, "\\\\") * 0.1)

  # Extract all rows within the tabular environment
  rows <- str_extract_all(latex_code, "(?<=\\\\\\\\)[^\\\\]+")

  # Remove LaTeX commands and symbols to isolate text content
  clean_rows <- sapply(rows, function(row) gsub("[&\\\\$^{}]", "", row))

  # Find the longest row by character count
  max_length <- max(nchar(clean_rows))

  # Estimate the average width of a character in inches (adjust as needed)
  average_char_width <- 0.1

  # Calculate the estimated maximum width of the table in inches
  estimated_table_width <- max_length * average_char_width


  writeLines(c("\\documentclass{article}",
               "\\usepackage[margin=1cm, paperwidth=", estimated_table_width, "in, paperheight=", height, "in]{geometry}",
               "\\begin{document}",
               latex_code,
               "\\end{document}"),
             path2)

  tools::texi2pdf(path2, clean = TRUE)

  pathf <- stringr::str_replace(string = path2, pattern = "\\.[^.]*$", replacement = ".pdf")

  fileend <- basename(pathf)

  file.rename(fileend, pathf)
}
