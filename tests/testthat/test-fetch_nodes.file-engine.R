library(testthat)
library(assertthat)

test_that("fetch_nodes file_engine works with basid id query", {
    #testthat::skip("temporary skip")

		filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
		e <- file_engine(filename)

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

test_that("fetch_nodes file_engine works with complex query syntax", {
		filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
		e <- file_engine(filename)

    # fetch_nodes(id %in% c("MONDO:0007525", "MONDO:0007526")) actually does work with file_engine
    g <- e %>% fetch_nodes(id %in% c("MONDO:0007525", "MONDO:0007526"))
    nodes_df <- g %>% activate(nodes) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 2)

    # basic single-id fetch
    g <- e %>% fetch_nodes(id == "MONDO:0007525")

    nodes_df <- g %>% activate(nodes) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 1)
    expect_equal(nodes_df$id, "MONDO:0007525")

    # test a big fetch
    g <- e %>% fetch_nodes("biolink:Disease" %in_list% category | "biolink:Gene" %in_list% category)
    nodes_df1 <- g %>% activate(nodes) %>% as.data.frame()
    expect_equal(nrow(nodes_df1), 133)
})

test_that("fetch_nodes limit works with file_engine", {
	filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")

	e <- file_engine(filename)
	g <- e %>% fetch_nodes("biolink:Disease" %in_list% category, limit = 10)

	nodes_df <- g %>% activate(nodes) %>% as.data.frame()
	expect_equal(nrow(nodes_df), 10)

	e <- file_engine(filename)
	g <- e %>% fetch_nodes("biolink:Disease" %in_list% category, limit = 5)

	nodes_df <- g %>% activate(nodes) %>% as.data.frame()
	expect_equal(nrow(nodes_df), 5)
})

