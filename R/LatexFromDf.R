#' LatexFromDf
#'
#' Transform and save a dataframe as a Latex Pdf and a csv copy in anycase. save pdf in the same doc.
#' @param df A DataFrame.
#' @param path The path where to save the new pdf.
#' @return Nothing - but create a pdf doc with the table in the righ path
#' @examples
#' data_summary(c(1, 2, 3, 4, 5))
#' @export

LatexFromDf <- function(df, path){
  write.csv(df, file = paste(path, ".csv", sep = ""))
    path2 <- paste(path, ".tex", sep = "")
    kableExtra::kable(df, "latex", booktabs = TRUE) %>%
      kableExtra::kable_styling(latex_options = "HOLD_position") %>%
      as.character() %>%
      {paste(
        "\\documentclass[varwidth, border=5pt]{standalone}",
        "\\usepackage{booktabs}",
        "\\usepackage{array}",
        "\\usepackage{multirow}",
        "\\usepackage[table]{xcolor}",
        "\\usepackage{colortbl}",
        "\\usepackage{pdflscape}",
        "\\usepackage{tabu}",
        "\\usepackage{threeparttable}",
        "\\usepackage{threeparttablex}",
        "\\usepackage[normalem]{ulem}",
        "\\usepackage{makecell}",
        "\\usepackage{xcolor}",
        "\\begin{document}",
        as.character(.),
        "\\end{document}",
        sep = "\n"
      )} %>%
      writeLines(path2)
    pdf_path <- gsub(".tex", ".pdf", path2)
    tryCatch({
      original_wd <- getwd()
      setwd(dirname(path2))
      tools::texi2pdf(basename(path2), clean = TRUE)
      setwd(original_wd)
    }, error = function(e) {
      setwd(original_wd)
      cat("Error in compiling PDF: ", e$message, "\n")
    })
  }
