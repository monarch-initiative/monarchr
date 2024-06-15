library(testthat)
library(assertthat)

test_that("search_nodes works for the monarch engine", {
    # skip for now
    #testthat::skip("temporary skip")

    e <- monarch_engine()
    g <- search_nodes(e, "fibrosis", category = "biolink:Disease", limit = 5)

    # ok, there should be 5 nodes, and 0 edges
    nodes_df <- g %>% nodes()
    expect_equal(nrow(nodes_df), 5)
    edges_df <- g %>% edges()
    expect_equal(nrow(edges_df), 0)

    # all the nodes should have a pcategory of biolink:Disease
    expect_true(all(nodes_df$pcategory == "biolink:Disease"))

    # the nodes should have a name or description that contains "fibrosis"
    expect_true(all(grepl("fibrosis", nodes_df$name) | grepl("fibrosis", nodes_df$description)))
})