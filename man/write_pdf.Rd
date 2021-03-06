% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_pdf.R
\name{write_pdf}
\alias{write_pdf}
\title{write_pdf}
\usage{
write_pdf(url, outfile = NULL, outdir = ".", overwrite = FALSE)
}
\arguments{
\item{outdir}{If outdir is null, will save pdfs into the directory of book title.}

\item{overwrite}{Whether overwrite existing pdf?}
}
\description{
Save to pdf for wikipedia page
}
\examples{
url <- "https://en.wikipedia.org/wiki/Degrees_of_freedom_\%28statistics\%29"
write_pdf(url)
}
