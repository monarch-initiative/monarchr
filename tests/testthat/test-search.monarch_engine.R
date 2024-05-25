library(testthat)
library(assertthat)

test_that("search works for monarch engine", {
    # skip for now
    #testthat::skip("temporary skip")

    e <- monarch_engine()
    g <- search(e, "cystic fibrosis", field = "id", limit = 20)

    print(g)
})
