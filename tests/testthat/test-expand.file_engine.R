library(testthat)
library(assertthat)

test_that("expand file_engine works with transitive", {
    #testthat::skip("temporary skip")
    options(width = 150)

		filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
		e <- file_engine(filename)

    query_ids = c("MONDO:0007525", "MONDO:0007524")

    ##### Check basic OUT transitive
    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(predicate = "biolink:subclass_of", transitive = TRUE, direction = "out")
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 20)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 14)

    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 26)

    ##### Check basic OUT transitive with categories
    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(predicate = "biolink:subclass_of", transitive = TRUE, direction = "out", categories = "biolink:Disease")
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 14)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 14)

    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 20)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 20)


    ##### Check basic IN transitive
    query_ids = c("MONDO:0007525", "MONDO:0007524")
    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(predicate = "biolink:subclass_of", transitive = TRUE, direction = "in")
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 4)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 4)

    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 2)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 2)
})

test_that("fetch_Edges file_engine works", {
    #testthat::skip("temporary skip")
    options(width = 150)

		filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")

		e <- file_engine(filename)
    query_ids = c("MONDO:0007525", "MONDO:0007524", "MONDO:0007523")

    ##### Check basic OUT
    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(direction = "out")
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 162)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 6)
    expect_equal(sum(nodes_df$pcategory == "biolink:PhenotypicFeature"), 156)

    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 182)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 4)

    ##### Check basic IN
    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(direction = "in")
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 10)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 5)

    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 8)

    #### Check basic BOTH
    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(direction = "both")
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 169)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 8)
    expect_equal(sum(nodes_df$pcategory == "biolink:PhenotypicFeature"), 156)
    expect_equal(sum(nodes_df$pcategory == "biolink:Gene"), 3)

    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 190)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 6)

    # Check OUT with has_phenotype
    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(predicate = "biolink:has_phenotype", direction = "out")
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 159)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 3)

    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 175)

    ##### Check OUT with categories biolink:Disease
    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(categories = "biolink:Disease", direction = "out")
    # this result should have 6 nodes, all biolink:Disease
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 6)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 6)

    # there should be 4 edges, all subclass_of
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 4)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 4)

    ##### Check IN with categories biolink:Disease
    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(categories = "biolink:Disease", direction = "in")
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 5)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 5)

    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 2)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 2)

    ##### Check BOTH with categories biolink:Disease
    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(categories = "biolink:Disease", direction = "both")
    # this result should have 8 nodes, all biolink:Disease
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 8)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 8)

    # there should be 6 edges, all subclass_of
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 6)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 6)

    ##### Check OUT with predicates biolink:has_mode_of_inheritance and categories biolink:PhenotypicFeature
    g <- fetch_nodes(e, query_ids = c(query_ids, "HGNC:2201")) %>% expand(categories = "biolink:PhenotypicFeature", predicates = "biolink:has_mode_of_inheritance", direction = "out")
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 5)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 3)

    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 3)
})
