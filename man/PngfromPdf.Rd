% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PngfromPdf.R
\name{PngfromPdf}
\alias{PngfromPdf}
\title{PngfromPdf}
\usage{
PngfromPdf(path, resolution = 300)
}
\arguments{
\item{path}{Path to the folder with pdfs}

\item{resolution}{Resolution of the pngs}
}
\value{
Nothing - but create transform the pdfs in the folder into a png
}
\description{
Transform and save a dataframe as a Latex Pdf and a csv copy in anycase. save pdf in the same doc.
}
\examples{
PngfromPdf("path/to/pdf/folder")
}
