library(testthat)
library(assertthat)

test_that("cypher_query returns a graph object", {
    # skip for now
    #testthat::skip("temporary skip")

    e <- monarch_engine()
    g <- cypher_query(e, query = "MATCH (s) -[r]- (o) return s, r, o LIMIT 1")
    expect_s3_class(g, "tbl_kgx")
    nodes_df <- data.frame(tidygraph::activate(g, nodes))
    edges_df <- data.frame(tidygraph::activate(g, edges))
    # g should be a tidygraph with two rows in nodes and one in edges
    expect_equal(nrow(nodes_df), 2)
    expect_equal(nrow(edges_df), 1)
})

test_that("cypher_query works with multicypher queries", {
	# skip for now
	#testthat::skip("temporary skip")

	e <- monarch_engine()
	queries <- c("MATCH (n {id: 'MONDO:0007947'}) RETURN n",
							 "MATCH (n {id: 'MONDO:0017309'}) RETURN n",
							 "MATCH (n {id: 'MONDO:0020066'}) RETURN n")
	g <- cypher_query(e, queries = queries)
	expect_s3_class(g, "tbl_kgx")
	nodes_df <- data.frame(tidygraph::activate(g, nodes))
	edges_df <- data.frame(tidygraph::activate(g, edges))
	# g should be a tidygraph with two rows in nodes and one in edges
	expect_equal(nrow(nodes_df), 3)
	expect_equal(nrow(edges_df), 0)
})
