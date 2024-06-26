library(testthat)
library(assertthat)


test_that("monarch_engine works as expected", {
    #testthat::skip("temporary skip")

    e <- monarch_engine()
    g <- fetch_nodes(e, query_ids = "MONDO:0006043")
    # this should have 6 subtypes (two direct, four under one of the direct children)
    subtypes <- g %>% expand(direction = "in",
                                  predicates = "biolink:subclass_of",
                                  transitive = TRUE)

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 7)
    expect_equal(nrow(edges_df), 6)

    # there should be a pcategory col of type character
    expect_true("pcategory" %in% names(nodes_df))
    expect_true(is.character(nodes_df$pcategory))

    # there should be a category col of type list
    expect_true("category" %in% names(nodes_df))
    expect_true(is.list(nodes_df$category))
})
