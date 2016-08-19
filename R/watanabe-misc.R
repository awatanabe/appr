#' Checks whether the given object is an integer number
#'
#' Checks whether the given object is an integer number. This is differnet behavior from \code{\link[base]{is.integer}}, which checks whether the variable type is integer. This often will not be useful.
#'
#' @param	x	A vector to test for integerness
#' @return	A logical vector the same length as \code{x} in which the \code{i}th element is TRUE if the \code{i}th element of \code{x} is an integer.
#' @export

isInteger <- function(x){
	
	# Tester function to see if an element is an integer.
	tester <- function(x){
		
		if(is.numeric(x) == TRUE){
			x%%1 == 0
		}else{
			FALSE
		}
	}
	vapply(x, tester, TRUE, USE.NAMES = FALSE)
}