library(testthat)
library(assertthat)

test_that("set_engine attaches an engine retrievable by get_engine", {
    filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
    engine <- file_engine(filename)

    g <- set_engine(toy_graph(), engine)
    expect_identical(get_engine(g), engine)
})
