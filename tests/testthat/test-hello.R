test_that("hello function returns input value", {
	input <- "John"
	output <- hello(input)
	expect_equal(output, input)
})

test_that("hello function prints greeting message", {
	expect_output(hello("test"), "Hello, world!")
})
