library(testthat)
library(assertthat)

test_that("monarch_search works", {
    testthat::skip("temporary skip")
    g <- monarch_search("fanconi anemia", limit = 5)
    # result should be a tbl_kgx with 5 nodes and no edges
    expect_s3_class(g, "tbl_kgx")
    nodes_df <- data.frame(tidygraph::activate(g, nodes))
    edges_df <- data.frame(tidygraph::activate(g, edges))
    expect_equal(nrow(nodes_df), 5)
    expect_equal(nrow(edges_df), 0)

    # limit = 1 should work
    g <- monarch_search("fanconi anemia", limit = 1)
    # result should be a tbl_kgx with 1 node and no edges
    nodes_df <- data.frame(tidygraph::activate(g, nodes))
    edges_df <- data.frame(tidygraph::activate(g, edges))
    expect_equal(nrow(nodes_df), 1)
    expect_equal(nrow(edges_df), 0)

    # no hits should work
    g <- monarch_search("this is not a real search term")
    # this should return a graph with no nodes
    nodes_df <- data.frame(tidygraph::activate(g, nodes))
    edges_df <- data.frame(tidygraph::activate(g, edges))
    expect_equal(nrow(nodes_df), 0)
    expect_equal(nrow(edges_df), 0)


    })
