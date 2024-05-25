library(testthat)
library(assertthat)
library(tidyr)


test_that("monarch_engine works as expected", {
    #testthat::skip("temporary skip")

    e <- monarch_engine()
    g <- query_ids(e, "MONDO:0006043")
    # this should have 6 subtypes (two direct, four under one of the direct children)
    subtypes <- g %>% fetch_edges(direction = "in",
                                  predicates = "biolink:subclass_of",
                                  transitive = TRUE)

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 7)
    expect_equal(nrow(edges_df), 6)
})