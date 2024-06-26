library(testthat)
library(assertthat)


test_that("monarch_engine_check works as expected", {
    #testthat::skip("temporary skip")

    # we can't test for TRUE here, because this check my be run without a connection
    # so we'll just test that it returns logical
    expect_type(monarch_engine_check(warn = FALSE), "logical")
})
