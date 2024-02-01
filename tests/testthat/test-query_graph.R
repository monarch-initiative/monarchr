library(testthat)
library(assertthat)

test_that("query_graph returns a graph object", {
    g <- query_graph(query = "MATCH (s) -[r]- (o) return s, r, o LIMIT 1")
    expect_s3_class(g, "tbl_kgx")
    nodes_df <- data.frame(tidygraph::activate(g, nodes))
    edges_df <- data.frame(tidygraph::activate(g, edges))
    # g should be a tidygraph with two rows in nodes and one in edges
    expect_equal(nrow(nodes_df), 2)
    expect_equal(nrow(edges_df), 1)
})