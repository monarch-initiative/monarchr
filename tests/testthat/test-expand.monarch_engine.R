library(testthat)
library(assertthat)


test_that("paging works as expected", {
	# there should MONDO:0008678 has ~ 234 phenotypes
	g <- monarch_engine() |>
		fetch_nodes(query_ids = "MONDO:0008678") |>
		expand(categories = "biolink:PhenotypicFeature")

	g2 <- monarch_engine() |>
		fetch_nodes(query_ids = "MONDO:0008678") |>
		expand(categories = "biolink:PhenotypicFeature",
					 page_size = 100)

	expect_equal(nrow(nodes(g)), nrow(nodes(g2)))
	expect_equal(nrow(edges(g)), nrow(edges(g2)))

	g_node_ids <- nodes(g)$id
	g2_node_ids <- nodes(g2)$id

	expect_true(all(g_node_ids %in% g2_node_ids))
	expect_true(all(g2_node_ids %in% g_node_ids))

	g_edge_ids <- paste(edges(g)$subject, edges(g)$predicate, edges(g)$object)
	g2_edge_ids <- paste(edges(g2)$subject, edges(g2)$predicate, edges(g2)$object)

	expect_true(all(g_edge_ids %in% g2_edge_ids))
	expect_true(all(g2_edge_ids %in% g_edge_ids))
})


test_that("paging works as expected", {
	# there should MONDO:0008678 has ~ 234 phenotypes
	g <- monarch_engine() |>
		fetch_nodes(query_ids = "MONDO:0008678") |>
		expand(predicates = "biolink:subclass_of", direction = "out", transitive = TRUE)

	g2 <- monarch_engine() |>
		fetch_nodes(query_ids = "MONDO:0008678") |>
		expand(predicates = "biolink:subclass_of", direction = "out", transitive = TRUE, page_size = 10)

	expect_equal(nrow(nodes(g)), nrow(nodes(g2)))
	expect_equal(nrow(edges(g)), nrow(edges(g2)))

	g_node_ids <- nodes(g)$id
	g2_node_ids <- nodes(g2)$id

	expect_true(all(g_node_ids %in% g2_node_ids))
	expect_true(all(g2_node_ids %in% g_node_ids))

	g_edge_ids <- paste(edges(g)$subject, edges(g)$predicate, edges(g)$object)
	g2_edge_ids <- paste(edges(g2)$subject, edges(g2)$predicate, edges(g2)$object)

	expect_true(all(g_edge_ids %in% g2_edge_ids))
	expect_true(all(g2_edge_ids %in% g_edge_ids))
})

test_that("expand works as expected", {
    #testthat::skip("temporary skip")

    e <- monarch_engine()
    g <- fetch_nodes(e, query_ids = "MONDO:0006043")
    # this should have 6 subtypes (two direct, four under one of the direct children)
    subtypes <- g %>% expand(direction = "in",
                                  predicates = "biolink:subclass_of",
                                  transitive = TRUE)

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 7)
    expect_equal(nrow(edges_df), 6)

    # there are two in and one out edges (all subclass_of)
    subtypes <- g %>% expand(direction = "both")

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 4)
    expect_equal(nrow(edges_df), 3)

    # there are two in and one out edges (all subclass_of)
    subtypes <- g %>% expand(direction = "in")

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 3)
    expect_equal(nrow(edges_df), 2)

    # there are two in and one out edges (all subclass_of)
    subtypes <- g %>% expand(direction = "out")

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 2)
    expect_equal(nrow(edges_df), 1)

    # 8 outgoing biolink:has_phenotype to biolink:PhenotypicFeature
    # 1 incoming biolink:causes from biolink:Gene
    # 1 outgoing biolink:subclass_of to biolink:Disease
    # 1 outgoing biolink:has_mode_of_inheritance to biolink:PhenotypicFeature

    g <- fetch_nodes(e, query_ids = "MONDO:0012187")
    phenos <- g %>% expand(predicates = "biolink:has_phenotype",
                                categories = "biolink:PhenotypicFeature")
    expect_equal(phenos %>% activate(edges) %>% data.frame() %>% nrow(), 8)

    ancestors <- g %>% expand(direction = "out", predicates = "biolink:subclass_of")
    expect_equal(ancestors %>% activate(edges) %>% data.frame() %>% nrow(), 1)

    # subclass or mode of inheritance
    outs <- g %>% expand(direction = "out", predicates = c("biolink:subclass_of", "biolink:has_mode_of_inheritance"))
    expect_equal(outs %>% activate(edges) %>% data.frame() %>% nrow(), 2)

    # should be 9 phenotypic feature result nodes
    phenos <- g %>% expand(categories = "biolink:PhenotypicFeature")
    expect_equal(phenos %>% activate(nodes) %>% data.frame() %>% nrow(), 10)

    # whole neighborhood should be 11 nodes
    neighborhood <- g %>% expand()
    expect_equal(neighborhood %>% activate(nodes) %>% data.frame() %>% nrow(), 12)

    # there should be 57 phenotypic features connected to this disease and its 2 subtypes
    # check in neo4j: MATCH (p0)<-[r0:`biolink:has_phenotype`]-(n)<-[r:`biolink:subclass_of`*]-(q)-[r2:`biolink:has_phenotype`]-(p)  WHERE n.id IN ["MONDO:0009242"]  RETURN p0, r0, n, r, q, r2, p
    g <- fetch_nodes(e, query_ids = "MONDO:0009242")
    with_subtypes <- g %>% expand(direction = "in", predicates = "biolink:subclass_of", transitive = TRUE)
    expect_equal(with_subtypes %>% activate(nodes) %>% data.frame() %>% nrow(), 3)

    phenos <- with_subtypes %>% expand(predicates = "biolink:has_phenotype",
                                            categories = "biolink:PhenotypicFeature")

    phenos_only <- phenos %>% activate(nodes) %>% data.frame() %>% rowwise() %>% filter("biolink:PhenotypicFeature" %in% category)
    expect_equal(phenos_only %>% nrow(), 55)
})
