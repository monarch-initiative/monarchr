library(testthat)
library(assertthat)
library(tidyr)

test_that("fetch_nodes neo4j works with basid id query", {
    #testthat::skip("temporary skip")

    e <- monarch_engine()

    # fetch_nodes(id %in% c("MONDO:0007525", "HGNC:4635")) should result in an error
    # do so silently in the logs...
    g <- fetch_nodes(e, id = c("MONDO:0007525", "HGNC:4635"))

    nodes_df <- g %>% activate(nodes) %>% as.data.frame()
    # there should be an id column with 2 entries: MONDO:0007525 and HGNC:4635,
    # but we can't gaurantee the order
    expect_equal(nrow(nodes_df), 2)
    expect_true(all(nodes_df$id %in% c("MONDO:0007525", "HGNC:4635")))

    # there should be no edges
    edges_df <- g %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(edges_df), 0)
})

test_that("fetch_nodes neo4j works with complex query syntax", {
    # fetch_nodes(id %in% c("MONDO:0007525", "HGNC:4635")) should result in an error
    # do so silently in the logs...
    expect_error(e %>% fetch_nodes(id %in% c("MONDO:0007525", "HGNC:4635")))

    e <- monarch_engine()
    g <- e %>% fetch_nodes(id == "MONDO:0007525")

    nodes_df <- g %>% activate(nodes) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 1)
    expect_equal(nodes_df$id, "MONDO:0007525")

    # test the limit parameter
    g <- e %>% fetch_nodes("biolink:Disease" %in% category | "biolink:Gene" %in% category, limit = 5)
    nodes_df1 <- g %>% activate(nodes) %>% as.data.frame()
    expect_equal(nrow(nodes_df1), 5)

    # test the skip parameter
    g <- e %>% fetch_nodes("biolink:Disease" %in% category | "biolink:Gene" %in% category, limit = 5, skip = 5)
    nodes_df2 <- g %>% activate(nodes) %>% as.data.frame()
    expect_equal(nrow(nodes_df2), 5)

    # the two queries should return different nodes
    expect_false(all(nodes_df1$id %in% nodes_df2$id))

    # check to see that we can chain the fetch_nodes function with other functions
    g <- e %>% 
      fetch_nodes(id == "MONDO:0007525") %>%
      fetch_edges(result_categories = "biolink:Gene")

    # this result should have 3 nodes and 3 edges
    nodes_df <- g %>% activate(nodes) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 3)

    edges_df <- g %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(edges_df), 3)
})
