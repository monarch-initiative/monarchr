library(testthat)
library(assertthat)

test_that("summary() for file_engine", {
    filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
    engine <- file_engine(filename)

    res <- summary(engine, quiet = TRUE)

    # make sure the output is a list
    expect_type(res, "list")

    # let's try a version where we capture the printed output
    printed <- capture.output(summary(engine, quiet = FALSE))

    # the result should be a character vector
    expect_type(printed, "character")
    # one of the lines should be "Total nodes: "
    expect_true(any(grepl("Total nodes: ", printed)))
})
