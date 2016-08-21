#' Sets default values
#' 
#' Called when a user attaches the package
#' @param libname	Path to the directory where the package is installed
#' @param pkgname	Name of the package
#' @return No return value
#' @export

.onLoad <- function(libname, pkgname){
	
	op <- options()
	opAppr <- list(
		apprPath = "/Users/aaronwatanabe/Code/appr"
	)
	
	toset <- !(names(opAppr) %in% names(op))
	if(any(toset)) options(opAppr[toset])
	
	invisible()
}