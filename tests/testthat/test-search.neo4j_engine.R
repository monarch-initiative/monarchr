library(testthat)
library(assertthat)

test_that("search works for neo4j graphs", {
    # skip for now
    #testthat::skip("temporary skip")

    e <- neo4j_engine()
    g <- search.neo4j_engine(e, "MONDO:0007525", field = "id", limit = 1)

    nodes_df <- g %>% nodes()
    expect_equal(nrow(nodes_df), 1)

    g <- search.neo4j_engine(e, "MONDO:.*", field = "id", limit = 10)

    nodes_df <- g %>% nodes()
    expect_equal(nrow(nodes_df), 10)
})
