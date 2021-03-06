% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/watanabe-df-manipulation.R
\name{mergeCols}
\alias{mergeCols}
\title{Merges one column from \code{sourceDF} into \code{df}}
\usage{
mergeCols(df, sourceDF, colName, dfIndex = "index", sourceIndex = "index",
  destName = colName)
}
\arguments{
\item{df}{Data frame to receive column}

\item{sourceDF}{Data frame containing column}

\item{colName}{String. Name of column in \code{sourceDF} to merge into \code{df}}

\item{dfIndex}{String. Name of index column in \code{df} to merge on . Must uniquely identify each row in \code{df} and have a one to one correspondence with \code{sourceIndex}.}

\item{sourceIndex}{String. Name of index column in \code{sourceDF} to merge on. Must uniquely identify each row in \code{sourceDF} and have a one to one correspondence with \code{dfIndex}.}

\item{destName}{String. Name column should take when merged into \code{df}.}
}
\value{
The data frame \code{df} containing \code{colName} from \code{sourceDF} named as \code{destName}.
}
\description{
Merges one column from \code{sourceDF} into \code{df} based on teh common index of the two data frames
}

