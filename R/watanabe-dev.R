packageSrcDir <- "/Users/aaronwatanabe/Dropbox/Academic Documents/R Personal Package"

#' Create package 'appr' from component files
#' 
#' Development function to build the package from the working files. Does not run tests
#' @param	srcEnviron		Location of directory containing raw code. 
#' @param 	srcFunctions	Name of directory containing package functions
#' @param   srcTests	 	Name of directory containing testthat files
#' @param	package			Location of directory in which to build package
#' @param	dev				Logical. True if include developer functions in package build
#' @return 	Returns \code{TRUE} on successfully building the package
#' @export

buildAppr <- function(
	srcEnviron = 
		"/Users/aaronwatanabe/Dropbox/Academic Documents/appr",
	srcFunctions = "functions",
	srcTests = "tests",
	package = "/Users/aaronwatanabe/Documents/codeDev/appr",
	dev = TRUE){
		
	# Save current working directory and return there on exit
	originalDir <- getwd()
	on.exit(setwd(originalDir))
		
	# Append src directories if environment is given
	if(is.character(srcEnviron) == TRUE){
		srcFunctions <- paste(srcEnviron, srcFunctions, sep = "/")
		srcTests <- paste(srcEnviron, srcTests, sep = "/")	
	}
	
	# Paths to sub-directories in package
	RDir 		<- paste(package, "R",     sep = "/")
	manDir 		<- paste(package, "man",   sep = "/")
	testsDir	<- paste(package, "tests/testthat", sep = "/")
	
	# Function directory name
	srcFuncDir <- basename(srcFunctions)
	
	# Get lists of current package contents
	oldFiles <- list(
		"package" = list.files(package),
		"man"     = list.files(manDir),
		"R"       = list.files(RDir),
		"tests"   = list.files(testsDir))

	# Set working directory as the package folder
	setwd(package)

	# Clear old files -- DO NOT EDIT PACKAGE FILES DIRECTLY
	unlink(
		oldFiles$package[oldFiles$package != "README.md"], # Keep README.md file
		recursive = TRUE)

	# Version Number
	majorVersion = 0
	minorVersion = 1
	workingVersion = 0
	buildNumber = strftime(Sys.time(), "%Y%m%d%H%M%S" ) # Date and time of creation	
	
	devtools::setup(
		path = package,
		description = list(
			"Title"     = "APPR (Aaron's Personal Package for R)",
			"Version"   = paste(
				majorVersion, 
				minorVersion, 
				workingVersion, 
				buildNumber,
				sep = "."),
			"Authors@R" = "person(given = 'Aaron', family = 'Watanabe', email = 'awatanabe@alumni.harvard.edu', role = c('aut', 'cre'))",
			"Description"  =  "A package of simple helper functions that I find useful. General move towards standardizing the R syntax that I use and moving towards immutability -- nothing makes code harder to understand that functions which edit objects I wasn't expecting.",
			"License" = "GPL-3"),
		check = FALSE,
		rstudio = FALSE)	
		
	# State libraries in use
	devtools::use_package("foreign", type = "Imports", pkg = package)
	devtools::use_package("utils",   type = "Imports", pkg = package)
	devtools::use_testthat(pkg = package)
	
	# Packages for developer tools
	if(dev == TRUE){
		devtools::use_package("devtools", type = "Imports", pkg = package)
		devtools::use_package("roxygen2", type = "Imports", pkg = package)
	}
	
	# Move function source code into R directory
	file.copy(from = list.files(srcFunctions, full.names = TRUE), to = RDir)

	# Move testing file into package directory
	file.copy(from = list.files(srcTests, full.names = TRUE), to = testsDir)
	
	roxygen2::roxygenize() # Use rather than devtools::document(), which has undocumented side effects (documented functions are loaded into active memory)

	# Check package
	devtools::check(document = FALSE) # Documentation deactivated due to side effect noted above
	
	# cat("Building package 'appr'\n", 
		# "     Version: ", paste(majorVersion, minorVersion, workingVersion, sep = "."), "\n",
		# "Build Number: ", buildNumber, "\n", 
		# sep = "")
	
	
	# # Load necessary libraries
	# cat("Loading required libraries:\n--- devtools..........")
	# require("devtools")
	# cat(" DONE\n--- roxygen2..........")
	# require("roxygen2")
	# cat(" DONE\n")
	
	# Setting 
	
}

#' Commit stage files in Git
#' 
#' Commits the currently staged files to Git repository. By default, updates documentation and checks package before committing, including running tests.
#' 

apprCommit(){
	
	
}

