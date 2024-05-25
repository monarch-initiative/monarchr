library(testthat)
library(assertthat)

test_that("search works for neo4j graphs", {
    # skip for now
    #testthat::skip("temporary skip")

    e <- neo4j_engine(url = "https://neo4j.monarchinitiative.org")
    g <- search_kg(e, "MONDO:0007525", field = "id", limit = 1)

    nodes_df <- g %>% nodes()
    expect_equal(nrow(nodes_df), 1)

    g <- search_kg(e, "MONDO:.*", field = "id", limit = 10)

    nodes_df <- g %>% nodes()
    expect_equal(nrow(nodes_df), 10)
})
