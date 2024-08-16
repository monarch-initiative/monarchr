library(testthat)
library(assertthat)


test_that("we can load data from url with file_engine", {
		filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")

    e <- file_engine(filename)

    # the result should have class file_engine
    expect_true(inherits(e, "file_engine"))
    # and a filename attribute
    expect_true("filename" %in% names(e))
    # and a graph attribute
    expect_true("graph" %in% names(e))

    # the graph attribute should be a tbl_kgx
    expect_true(inherits(e$graph, "tbl_kgx"))

    # the nodes df should have a description field for this test of type character
    nodes_df <- nodes(e$graph)
    expect_true("description" %in% names(nodes_df))

    # the edges df should have a knowledge_source field for this test of type character
    edges_df <- edges(e$graph)
    expect_true("knowledge_source" %in% names(edges_df))

    # the nodes df category should be a list column
    expect_true(is.list(nodes_df$category))

    # description should not be a list col
    expect_true(!is.list(nodes_df$description))
})
