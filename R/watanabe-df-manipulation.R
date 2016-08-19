#' Adds a new column to data frame
#'
#' Adds a column to a dataframe. New column will be the last column in the data frame. Note, this should not be used to add columns from another data frame.
#' @param 	df		A data frame
#' @param 	colName		Character vector. Name for the new column. If NULL, make.names() will be used to give the column a name.
#' @param	colData		Vector. Must have same number of elements as the rows in df containing the data for the new column.
#' @param	overwrite	Boolean. Overwrite any existing data in the column named. Ignored if \code{colName} is not specified.
#' @return 	The data frame with a new column added.
#' @export

addCol <- function(df, colData, colName = NULL, overwrite = FALSE){
	
	# Check df is a data frame
	if(is.data.frame(df) == FALSE){
		stop("Object passed to addCol as argument 'df' is not a data frame")
	}	
	
	# Reject if the new vector is not the proper length
	if(length(colData) != nrow(df)){
		stop("Vector passed as new column has ", length(colData), " row(s) while the data frame it is being added to has ", nrow(df), " rows. Vector and data frame must be the same length.")
	}
	
	# Reject if new column's name is already in the data frame and overwrite is not allowed
	if(is.element(colName, names(df)) == TRUE && overwrite == FALSE){
		stop(colName, " is already a column in the data frame passed to df")
	}
	
	if(is.null(colName) == TRUE ){
		colName <- make.names(colData)
	}
		
	df[[colName]] <- colData
	df	
}

#' Drops a column
#' 
#' Removes a column from a data frame
#' @param 	df			A data frame
#' @param	...			Objects which can be coerced to a vector of the column names to be dropped
#' @return	The data frame \code{df} without column \code{colName}
#' @export

dropCol <- function(df, ...){
	
	dropCols <- unlist(list(...), recursive = TRUE)
	
	# Check df is a data frame
	if(is.data.frame(df) == FALSE){
		stop("Object passed to dropCol as argument 'df' is not a data frame")
	}		
	
	# Check that colName is a column in df
	if(all(dropCols %in% names(df)) == FALSE){
		stop(setdiff(names(df), dropCols), " is/are not in df and cannot be dropped")

	}

	df[,!(names(df) %in% dropCols), drop = FALSE]
	
}


#' Renames column in data frame
#' 
#' Wrapper function to make column renaming easier.
#' @param df       A dataframe.
#' @param oldName  The name of the column to be renamed.
#' @param newName  The new name for the column.      
#' @return The dataframe with the column renamed.
#' @export

renameCol <- function(df, oldName = "", newName = ""){
	
	# Check original column name exists
	if(is.element(oldName, names(df)) == FALSE){
		stop("Column name ", oldName, " not found in dataframe and therefore cannot be renamed.")
	}
	
	# Check that new column name does not already exist
	if(is.element(newName, names(df)) == TRUE){
		stop("Column ", oldName, " cannot be renamed to '", newName, "' because '", newName, "' is already a column in the data frame passed to renameCol().")
	}
	
	names(df)[names(df) == oldName] <- newName
	df	
}

#' Maps function onto each element of the specified columns
#' 
#' Maps \code{fun} to each element of the the columns of \code{df} specified in \code{...}. Returns the original data frame with the values of specified columns updated with the values produced from mapping the function
#' @param df    A dataframe.
#' @param fun   A function to apply to each element of the selected columns. Elements of these columns will be the first argument passed to this function.
#' @param cols  A vector containing the names (as strings) or index numbers of the columns with elements to which to apply \code{fun}.
#' @param ...   Additional arguments to pass to fun
#' @return      A dataframe of the same dimensions as \code{df} with \code{cols} containing the results of applying \code{fun} to each element.
#' @export 
#' @section Details:
#' \code{mapCols} works by folding over the set of columns to manipulate. For each column in \code{cols}, runs \code{fun} over its elements using sapply and replacing the column's original contents with the output.

mapCols <- function(df, fun, cols = c(), ...){
	
	# Wrapper function to apply function
	wrapFun <- function(dfAccum, colName){
		dfAccum[[colName]] <- sapply(dfAccum[[colName]], fun, ...,  USE.NAMES = FALSE)
		dfAccum
	}
	
	# Fold over list of columsn
	Reduce(wrapFun, cols, df)
	
}

#' Merges one column from \code{sourceDF} into \code{df}
#'
#' Merges one column from \code{sourceDF} into \code{df} based on teh common index of the two data frames
#' @param	df			Data frame to receive column
#' @param	sourceDF	Data frame containing column
#' @param	colName		String. Name of column in \code{sourceDF} to merge into \code{df}
#' @param	dfIndex		String. Name of index column in \code{df} to merge on . Must uniquely identify each row in \code{df} and have a one to one correspondence with \code{sourceIndex}.
#' @param	sourceIndex	String. Name of index column in \code{sourceDF} to merge on. Must uniquely identify each row in \code{sourceDF} and have a one to one correspondence with \code{dfIndex}.
#' @param	destName	String. Name column should take when merged into \code{df}.
#' @return	The data frame \code{df} containing \code{colName} from \code{sourceDF} named as \code{destName}.
#' @export
mergeCols <- function(df, sourceDF, colName, dfIndex = "index", sourceIndex = "index", destName = colName){
	stop("To implement")
}


#' Searchs given data frame for rows in which the given column has the specified value
#' 
#' Searchs dataframe \code{df} for rows containing \code{colValue} in \code{col}. Returns an index vector (i.g. logical vector that is the legnth of the number of rows of \code{df}) where the \code{i}th element is TRUE if the \code{i}th row of \code{df} meets the search criterion
#' @param	df			A data frame to search
#' @param	col			Name or index of column to search for \code{colValue}.
#' @param 	colValue		Value in \code{column} to search for. Must be the type of \code{col}.
#' @export

searchDF <- function(df, col, colValue){
	
	# Check df is a data frame
	if(is.data.frame(df) == FALSE){
		stop("Object passed to searchDF as argument 'df' is not a data frame.")
	}
	
	# Check that col is either a string or an integer
	if(is.character(col) == FALSE && appr::isInteger(col) == FALSE){
		stop("Argument 'col' passed to searchDF must be column name or index of given data frame.")
	}
	
	# Check that col is a valid column name, if string
	if(is.character(col) == TRUE && is.element(col, names(df)) == FALSE){
		stop(col, " is not a column in data frame passed to searchDF.")
	}
	
	# Check that col is valid index, if integer
	if(appr::isInteger(col) == TRUE && (col < 0 || col > ncol(df))){
		stop("Column index passed to searchDF is not valid. Index is ",col," when the data frame only has ", ncol(df), " columns.")
	}
	
	df[,col] == colValue	
	
}

#' Replace values in columns in specified rows
#' 
#' Updates named column(s) in \code{df} with given value(s) for the rows indicated by \code{searchIndex} and returns new copy of \code{df}.
#' @param df			A dataframe
#' @param searchIndex	A logical vector with length equal to the number of rows in \code{df} in which the \code{i}th element is 'TRUE' if the \code{i}th row in \code{df} should be subject to replacement. Can be generated using \code{\link{searchDF}}. 
#' @param ...           Replacement columns and values, passed as consequetive arguments with the column name as the first argument and the new value as the second. 
#' @return A new copy up \code{df} with the specified rows and columns updated.
#' @export

replaceValues <- function(df, searchIndex, ...){

	# Check df is a data frame
	if(is.data.frame(df) == FALSE){
		stop("Object passed to replaceValues as argument 'df' is not a data frame.")
	}

	# Get out replacement paramters
	replacementParams = list(...)

	# Prepare named list with column names and replacement values
	colNames  <- unlist(replacementParams[seq(1,length(replacementParams), 2)]) # Column names
	newValues <-        replacementParams[seq(2,length(replacementParams), 2)]  # New values	
	
	df[searchIndex, colNames] <- newValues
	
	df
	
}

#' Carries out multiple replacements of values in a dataframe
#' 
#' @param df	    The data frame to update. 
#' @param updates	A dataframe containing information that identifies columns and their replacement values as well as which rows should be updated. Format is specified below.
#' @section Update Table:
#' The update table contains information about which rows to update and the columns in \code{df} and values they should be updated with. 

updateDF <- function(df, updates){
	
	# Constants
	searchColPrefix  <- "OLD"
	replaceColPrefix <- "NEW"
	
	# Check df is a data frame
	if(is.data.frame(df) == FALSE){
		stop("Object passed to updateDF as argument 'df' is not a data frame.")
	}	
	
	# Check updateTable is a data frame
	if(is.data.frame(updates) == FALSE){
		stop("Object passed to updateDF as argument 'updates' is not a data frame.")
	}		
	
	# Get search and replacement column names
	colNames <- names(updates)
	
	searchColsUpdates <- Filter(function(x) substr(x, 1, nchar(searchColPrefix)) == searchColPrefix,  colNames)
	replaceColsUpdates <- Filter(function(x) substr(x, 1, nchar(replaceColPrefix)) == replaceColPrefix,  colNames)
	
	searchCols  <- unlist(Map(function(x) sub(paste("^", searchColPrefix, sep = ""), "", x), searchColsUpdates))
	replaceCols <- unlist(Map(function(x) sub(paste("^", replaceColPrefix, sep = ""), "", x), replaceColsUpdates))
	
	replacer <- function(updated, r){
		
		
		
	}
	
	
	replaceValues(df, )
	
	df
}


#' Recodes elements of a column according to a conversion table
#' 
#' Recodes elements of a column according to a conversion table and writes them to a new column. Will resort the rows in \code{df}.
#' 
#' @param df   			A data frame
#' @param colName 		Name of column to code
#' @param convertTable 	Data frame containing conversation scores. Should contain two columns: oldValue and newValue. The table must have as many rows as there are unique values in \code{colName}. Each row should correspond an oldValue to the newly coded value.
#' @param destName		Character vector. Name of new column to write recoded data to. May not already ben the name of a column in \code{df}. If NULL, recoded data will be written over the original contents in \code{colName}.
#' @return The original data frame with the new column \code{scoreName} containing the results of scoring \code{colName} with \code{scoreTable}
#' @export

recodeCol <- function(df, colName, convertTable, destName = NULL){

	# Constants
	ctOldValue <- "oldValue"
	ctNewValue <- "newValue"

	# Check df is a data frame
	if(is.data.frame(df) == FALSE){
		stop("Object passed to recodeCol as argument 'df' is not a data frame.")
	}	
	
	# Check if colName is a valid column name
	if(is.element(colName, names(df)) == FALSE){
		stop(colName, " is not the name of a column in data frame passed to recodeCol.")
	}
	
	# Check if convertTable has correct column names
	if(setequal(names(convertTable), c(ctOldValue, ctNewValue)) == FALSE){
		stop("Conversion table passed to recodeCol has column names: ", names(convertTable), ". Should have names: ", ctOldValue, " and ", ctNewValue)
	}
	
	# Check if convertTable has full mapping of columns
	if(all(df[[colName]] %in% convertTable[[ctOldValue]]) == FALSE){
		stop("Conversion table passed to recodeCol does not have mappings for all values of ", colName, ". Missing values: ", df[[colName]][(!df[[colName]] %in% convertTable[[ctOldValue]])])
	}

	overwrite <- FALSE # Put recoded data back into original column

	# Enforces valid destination column name
	if(is.null(destName) == TRUE){
		overwrite <- TRUE
		
		# Finds a valid placeholder name
		tempNames <- make.unique(c(names(df), paste(ctNewValue, "1", sep=".")))
		destName <- setdiff(tempNames, names(df))
		
		if(length(destName) != 1){
			stop("Error occurred finding unique name for temp column in recodeCol.")
		}
	}else{
		if(is.element(destName, names(df)) == TRUE){
			stop("Destination column name '", destName, "' for recodings of '", colName, "' is already a column in data frame. Pick new column name.")
	}}

	# Rename the newValue column to the name of the destination column
	convertTable <- appr::renameCol(convertTable, oldName = ctNewValue, newName = destName)

	df <- merge(df, convertTable, by.x = colName, by.y = ctOldValue)
	
	if(overwrite == TRUE){
		df <- appr::dropCol(df, colName)
		appr::renameCol(df, destName, colName)
		
	}else{
		df
	}
		
}