library(watanaber)
context("dropCol() tests")

testDF <- data.frame(
	"firstName" = c("John", "Walter", "Bob", "Will"), 
	"lastName" = c("Smith", "Brown", "Anderson", "Anderson"), 
	"age" = c(20, 24, 32, 21),
	stringsAsFactors = FALSE)
	
test_that("dropCol() returns an error",{
	expect_error(
		dropCol("Not a data frame.", "firstName"), 
		regexp = "Object passed to dropCol as argument 'df' is not a data frame")
	expect_error(
		dropCol(testDF, "Robert"))
	expect_error(
		dropCol(testDF, "firstName", "Robert"))
	expect_error(
		dropCol(testDF, "middleName", "Bob"))
	expect_error(
		dropCol(testDF, TRUE))
})	

test_that("dropCol() returns the correct data frame", {
	expect_equal(dropCol(testDF, "firstName"), data.frame( 
		"lastName" = c("Smith", "Brown", "Anderson", "Anderson"), 
		"age" = c(20, 24, 32, 21),
		stringsAsFactors = FALSE))
	expect_equal(dropCol(testDF, "firstName", "lastName"), data.frame(
		"age" = c(20, 24, 32, 21),
		stringsAsFactors = FALSE))
	expect_equal(dropCol(testDF, c("firstName", "lastName")), data.frame(
		"age" = c(20, 24, 32, 21),
		stringsAsFactors = FALSE))	
	expect_equal(dropCol(testDF, list("firstName", "lastName")), data.frame(
		"age" = c(20, 24, 32, 21),
		stringsAsFactors = FALSE))	
	expect_equal(dropCol(testDF, c("firstName"), "lastName"), data.frame(
		"age" = c(20, 24, 32, 21),
		stringsAsFactors = FALSE))	
	expect_equal(dropCol(testDF, list("firstName"), "lastName"), data.frame(
		"age" = c(20, 24, 32, 21),
		stringsAsFactors = FALSE))		
	expect_equal(dropCol(testDF, c("firstName"), list("lastName")), data.frame(
		"age" = c(20, 24, 32, 21),
		stringsAsFactors = FALSE))					
})
	