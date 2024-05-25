library(testthat)
library(assertthat)

test_that("search_kg works for monarch engine", {
    # skip for now
    #testthat::skip("temporary skip")

    e <- monarch_engine()
    g <- search_kg(e, "cystic fibrosis", category = "biolink:Disease", limit = 20)

    # the nodes df should have 20 rows
    nodes_df <- g %>% nodes()
    expect_equal(nrow(nodes_df), 20)
})
