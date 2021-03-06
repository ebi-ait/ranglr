% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{load_spreadsheet}
\alias{load_spreadsheet}
\title{Load a metadata spreadsheet}
\usage{
load_spreadsheet(path)
}
\arguments{
\item{path}{a string.}
}
\value{
a list of tibbles for each sheet in the spreadsheet.
}
\description{
\code{load_spreadsheet} loads a multi-sheet metadata spreadsheet.
}
\details{
It removes the top 3 rows as well as removing the 'fill below this line' row.

The names of the tibbles are the names of the sheets.

This function extends the recommended example at
https://readxl.tidyverse.org/articles/articles/readxl-workflows.html.
}
\examples{
load_spreadsheet("~/name_of_spreadsheet.xlsx")
}
