% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/watanabe-io.R
\name{loadSPSS}
\alias{loadSPSS}
\title{Load SPSS file}
\usage{
loadSPSS(filePath = "", factorLabels = FALSE, dfConvert = TRUE)
}
\arguments{
\item{filePath}{Path to SPSS file to load into memory.}

\item{factorLabels}{Convert variables with values as a factors with those levels.}

\item{dfConvert}{Boolean whether to convert file to dataframe. Otherwise returns as list.}
}
\value{
A dataframe with the contents of the SPSS file.
}
\description{
Wrapper function around \code{foreign::read.spss()} to load SPSS data files into a data frame. Specifies default values to facilitate process.
}

