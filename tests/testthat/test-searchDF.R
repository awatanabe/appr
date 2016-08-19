library(watanaber)
context("searchDF() tests")

testDF <- data.frame(
	"firstName" = c("John", "Walter", "Bob", "Will"), 
	"lastName" = c("Smith", "Brown", "Anderson", "Anderson"), 
	"age" = c(20, 24, 32, 21),
	stringsAsFactors = FALSE)
	
test_that("searchDF returns an error",{
	expect_error(
		searchDF("Not a data frame.", "firstName", "John"), 
		regexp = "Object passed to searchDF as argument 'df' is not a data frame.")
	expect_error(
		searchDF(testDF, 5.4, "Robert"),
		regexp = "Argument 'col' passed to searchDF must be column name or index of given data frame.")
	expect_error(
		searchDF(testDF, "middleName", "Robert"),
		regexp = "middleName is not a column in data frame passed to searchDF.")
	expect_error(
		searchDF(testDF, 4, "Robert"),
		regexp = "Column index passed to searchDF is not valid. Index is 4 when the data frame only has 3 columns.")
})	

test_that("searchDF returns the correct index vector", {
	expect_equal(searchDF(testDF, "firstName", "John"),    c(TRUE,  FALSE, FALSE, FALSE))
	expect_equal(searchDF(testDF, "firstName", "Jack"),    c(FALSE, FALSE, FALSE, FALSE))
	expect_equal(searchDF(testDF, "age",       32    ),    c(FALSE, FALSE, TRUE,  FALSE))
	expect_equal(searchDF(testDF, "lastName", "Anderson"), c(FALSE, FALSE, TRUE,  TRUE))
})
	