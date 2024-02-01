library(testthat)
library(assertthat)

test_that("monarch_search works", {
    g <- monarch_search("fanconi anemia", limit = 5)
    # result should be a tbl_kgx with 5 nodes and no edges
    expect_s3_class(g, "tbl_kgx")
    nodes_df <- data.frame(tidygraph::activate(g, nodes))
    edges_df <- data.frame(tidygraph::activate(g, edges))
    expect_equal(nrow(nodes_df), 5)
    expect_equal(nrow(edges_df), 0)
    })
