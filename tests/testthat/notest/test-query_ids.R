library(testthat)
library(assertthat)

test_that("query_ids returns a tbl_kgx graph", {
     #testthat::skip("temporary skip")

    ids <- c("MONDO:0015780", "HP:0001903", "HGNC:2890")
    e <- neo4j_engine()
    result <- query_ids(e, ids)
    expect_s3_class(result, "tbl_kgx")
    nodes_df <- data.frame(tidygraph::activate(result, nodes))
    edges_df <- data.frame(tidygraph::activate(result, edges))
    expect_equal(nrow(nodes_df), 3)
    expect_equal(nrow(edges_df), 0)

    ids <- "MONDO:0015780"
    result <- query_ids(e, ids)
    expect_s3_class(result, "tbl_kgx")
    nodes_df <- data.frame(tidygraph::activate(result, nodes))
    edges_df <- data.frame(tidygraph::activate(result, edges))
    expect_equal(nrow(nodes_df), 1)
    expect_equal(nrow(edges_df), 0)
})