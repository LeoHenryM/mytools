#' LatexFromDf
#'
#' Transform and save a dataframe as a Latex Pdf and a csv copy in anycase. save pdf in the same doc.
#' @param df A DataFrame.
#' @param path The path where to save the new pdf.
#' @param title a character title
#' @param subtitle a character subtitle - not working yet
#' @param endnotes a character endnotes
#' @return Nothing - but create a pdf doc with the table in the righ path
#' @examples
#' data_summary(c(1, 2, 3, 4, 5))
#' @export

LatexFromDf <- function(df, path, title = NULL, subtitle = NULL, endnotes = NULL, Width = NULL, Height = NULL){

  library(kableExtra)
  library(stringi)

  write.csv(df, file = paste(path, ".csv", sep = ""))

  path2 <- paste(path, ".tex", sep = "")

  # Create LaTeX code with kable and styling
  latex_code <- kable(df, "latex", booktabs = TRUE, caption = title) %>%
    kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

    if(is.null(subtitle)){

    }else{
      print("subtitle not working yet")
    }

  latex_code <- latex_code %>%
    row_spec(0, extra_latex_after = "\\midrule")

    if(is.null(endnotes)){

    }else{
      latex_code <- latex_code %>%
        footnote(general_title = "Notes:",
                 general = endnotes,
                 footnote_as_chunk = T)}

  latex_code <- latex_code %>%
    as.character()

  # Extract all rows within the tabular environment
  rows <- str_extract_all(latex_code, "(?<=\\\\\\\\)[^\\\\]+")
  # Remove LaTeX commands and symbols to isolate text content
  clean_rows <- sapply(rows, function(row) gsub("[&\\\\$^{}]", "", row))
  # Find the longest row by character count
  max_length <- max(nchar(clean_rows))
  # Estimate the average width of a character in inches (adjust as needed)
  average_char_width <- 0.15
  # Calculate the estimated maximum width of the table in inches
  estimated_table_width <- max_length * average_char_width

  height <- max(1, str_count(latex_code, "\\\\") * 0.1)

  if(is.null(Width)) estimated_table_width <- estimated_table_width else estimated_table_width <- Width
  if(is.null(Height)) height <- height else height <- Height

  # Split the LaTeX code into lines
  latex_lines <- strsplit(latex_code, "\n")[[1]]

  # Assemble the complete document using landscape orientation and possibly smaller font
  complete_latex <- paste(
    "\\documentclass{article}",  # Using article class for better control
    "\\usepackage[margin=1cm, paperwidth=", estimated_table_width, "in, paperheight=", height, "in]{geometry}",
    "\\usepackage{float}",  # Add this to support the 'H' position specifier
    "\\usepackage{booktabs}",
    "\\usepackage{array}",
    "\\usepackage{multirow}",
    "\\usepackage[table]{xcolor}",
    "\\usepackage{colortbl}",
    "\\usepackage{pdflscape}",  # Ensures landscape mode if needed
    "\\usepackage{tabu}",
    "\\usepackage{threeparttable}",
    "\\usepackage{threeparttablex}",
    "\\usepackage[normalem]{ulem}",
    "\\usepackage{makecell}",
    "\\usepackage{xcolor}",
    "\\begin{document}",
    "\\small",  # Use smaller font size to fit more content
    paste(latex_lines, collapse = "\n"),
    "\\end{document}",
    sep = "\n"
  )

  # Write the LaTeX document to file
  writeLines(complete_latex, path2)


    pdf_path <- gsub(".tex", ".pdf", path2)
    tryCatch({
      original_wd <- getwd()
      setwd(dirname(path2))
      tools::texi2pdf(basename(path2), clean = TRUE)
      setwd(original_wd)
      print("Pdf fait!")
    }, error = function(e) {
      setwd(original_wd)
      cat("Error in compiling PDF: ", e$message, "\n")
    })
  }
