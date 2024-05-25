library(testthat)
library(assertthat)
library(tidyr)

test_that("base_engine works", {
    # first, let's make sure the default preferences is loaded
    e <- base_engine()
    str(e)
    expect_equal(e$name, "default_engine")
    # result should have a category_priority field
    expect_true("category_priority" %in% names(e$preferences))

    # test that we can load preferences via a list
    e <- base_engine(preferences = list(category_priority = c("biolink:Gene", "biolink:Disease")))
    expect_equal(e$name, "default_engine")
    # result should have a category_priority field of length 2
    expect_equal(length(e$preferences$category_priority), 2)

})