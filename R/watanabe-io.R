#' Load SPSS file
#'
#' Wrapper function around \code{foreign::read.spss()} to load SPSS data files into a data frame. Specifies default values to facilitate process.
#' 
#' @param filePath      Path to SPSS file to load into memory.
#' @param dfConvert     Boolean whether to convert file to dataframe. Otherwise returns as list.
#' @param factorLabels  Convert variables with values as a factors with those levels.
#' @return A dataframe with the contents of the SPSS file.
#' @export

loadSPSS <- function (filePath = "", factorLabels = FALSE, dfConvert = TRUE){
	
	foreign::read.spss(filePath, use.value.labels = factorLabels, to.data.frame = dfConvert)
}

#' Load STATA file (pre-version 13)
#'
#' Wrapper function around \code{foreign::read.dta()} to load STATA data into memory. Will work only with files created compatable with STATA version 12 or before. Standardizes import options and sets preferred default values. 
#' 
#' @param filePath      Path to STATA file to load into memory.
#' @param factorLabels  Convert variables with values as a factors with those levels.
#' @return A dataframe with the contents of the SPSS file.
#' @export

loadDTA <- function (filePath = "", factorLabels = FALSE){
	
	foreign::read.dta(filePath, convert.factors = factorLabels)
}

#' Load CSV file
#' 
#' Wrapper function around utils::read.csv() to load CSV (comma separated values) files into memory as data frames. 
#' 
#' @param filePath      Path to CSV file to load into memory.
#' @param headers       Is the first row of the file column names?
#' @param factorLabels  Convert variables with values as a factors with those levels.
#' @param encodingFile  Encoding of the original file (UTF-8, macintosh, etc)
#' @param encoding      Marks imported strings as known to have a particular coding. Does not change the encoding itself, which will be automatically set to the encoding R is using on the local system. See 'Encoding.'
#' @param ...           Additional arguments to pass to \code{\link[utils]{read.csv}}.
#' @return              A data frame with the contents of the CSV file.
#' @section Encoding:
#' Encodings refer to the way that characters are stored in memory (or more specifically how R translates the zeros and ones in memory into characters). Unfortunately, there are many encoding systems. R will use one encoding, which \href{https://stat.ethz.ch/pipermail/r-sig-mac/2007-March/003733.html}{at least on macs is \code{UTF-8}}, but you may be reading data files from another. R will automatically convert data to its encoding system on import, but may need to know what those are. Use \code{encodingFile} to specify the encoding of the file you're importing from. Generally, you should not need to change \code{encoding}, unless R is using \code{Laitn-1} (perhaps the standard for Windows) or a different encoding on your machine.
#' @export

loadCSV <- function(filePath, headers = TRUE, factorLabels = FALSE, encodingFile = "UTF-8", encoding = "UTF-8", ...){

	utils::read.csv(file = filePath, header = headers, stringsAsFactors = factorLabels, fileEncoding = encodingFile, encoding = "UTF-8", ...)
}
