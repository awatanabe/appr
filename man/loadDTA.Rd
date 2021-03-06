% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/watanabe-io.R
\name{loadDTA}
\alias{loadDTA}
\title{Load STATA file (pre-version 13)}
\usage{
loadDTA(filePath = "", factorLabels = FALSE)
}
\arguments{
\item{filePath}{Path to STATA file to load into memory.}

\item{factorLabels}{Convert variables with values as a factors with those levels.}
}
\value{
A dataframe with the contents of the SPSS file.
}
\description{
Wrapper function around \code{foreign::read.dta()} to load STATA data into memory. Will work only with files created compatable with STATA version 12 or before. Standardizes import options and sets preferred default values.
}

