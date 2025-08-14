library(testthat)
library(assertthat)

test_that("we can load the example KG w/ data()", {
	data(eds_marfan_kg, envir = environment())

	e <- eds_marfan_kg

	# repeating tests from test-fetch_nodes.file_engine.R
	g <- fetch_nodes(e, query_ids = c("MONDO:0007525", "MONDO:0007526"))

	# ensure that the last_engine attribute is set
	expect_true(has_attr(g, "last_engine"))
	# and that it has the right class
	expect_true(inherits(attr(g, "last_engine"), "file_engine"))

	nodes_df <- g %>% activate(nodes) %>% as.data.frame()
	# there should be an id column with 2 entries: MONDO:0007525 and HGNC:4635,
	# but we can't gaurantee the order
	expect_equal(nrow(nodes_df), 2)
	expect_true(all(nodes_df$id %in% c("MONDO:0007525", "MONDO:0007526")))

	# there should be no edges
	edges_df <- g %>% activate(edges) %>% as.data.frame()
	expect_equal(nrow(edges_df), 0)
})


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
