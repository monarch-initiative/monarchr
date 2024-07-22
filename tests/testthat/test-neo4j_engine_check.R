library(testthat)
library(assertthat)


test_that("neo4j_engine_check works as expected", {
    #testthat::skip("temporary skip")

    # we can't test for TRUE here, because this check my be run without a connection
    # so we'll just test that it returns logical
    expect_type(neo4j_engine_check("https://neo4j.monarchinitiative.org", warn = FALSE), "logical")
    # we can test for FALSE however
    expect_false(neo4j_engine_check("https://no-such-db.monarchinitiative.org", warn = FALSE))
})
