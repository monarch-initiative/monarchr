library(testthat)
library(assertthat)

test_that("summary() for neo4j_engine", {
    res <- summary(monarch_engine(), quiet = TRUE)

    # make sure the output is a list
    expect_type(res, "list")

    # let's try a version where we capture the printed output
    printed <- capture.output(summary(monarch_engine(), quiet = FALSE))

    # the result should be a character vector
    expect_type(printed, "character")
    # one of the lines should be "Total nodes: "
    expect_true(any(grepl("Total nodes: ", printed)))
})
