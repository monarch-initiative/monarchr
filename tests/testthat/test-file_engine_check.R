library(testthat)
library(assertthat)


test_that("file_engine_check works as expected", {
    #testthat::skip("temporary skip")

		filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
		expect_true(file_engine_check(filename, warn = FALSE))

    filename <- system.file("extdata", "nosuch_kgx_tsv.tar.gz", package = "monarchr")
    expect_false(file_engine_check(filename, warn = FALSE))

    # we can try to connect to a remote file, but we can't test for TRUE here
    # because this check my be run without a connection
    # so we'll just test that it returns logical
    expect_type(file_engine_check("https://no-such-host.kghub.io/kg-obo/dummy/dummy_kgx_tsv.tar.gz", warn = FALSE), "logical")
})
