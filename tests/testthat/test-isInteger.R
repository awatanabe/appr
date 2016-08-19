library(watanaber)
context("isInteger() tests")

test_that("isInteger() returns the correct values", {
	expect_true(isInteger(1))
	expect_true(isInteger(-1))
	expect_false(isInteger(1.2))
	expect_false(isInteger(5.0000001))
	expect_false(isInteger("string"))
	expect_false(isInteger(TRUE))
	expect_equal(isInteger(c(1,2)),		c(TRUE, TRUE))
	expect_equal(isInteger(c(1,1.2)),	c(TRUE, FALSE))
})
	