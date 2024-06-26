library(testthat)
library(assertthat)
library(tidyr)

test_that("fetch_edges with no return results works", {
    eds_hits <- query_ids(c("WB:WBGene00008083")) # no associations
    g <- eds_hits %>% fetch_edges(result_categories = "biolink:Disease")
})

test_that("fetch_edges works as expected", {
    testthat::skip("temporary skip")

    g <- query_ids("MONDO:0006043")
    # this should have 6 subtypes (two direct, four under one of the direct children)
    subtypes <- g %>% fetch_edges(direction = "in",
                                  predicates = "biolink:subclass_of",
                                  transitive = TRUE)

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 7)
    expect_equal(nrow(edges_df), 6)

    # there are two in and one out edges (all subclass_of)
    subtypes <- g %>% fetch_edges(direction = "both")

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 4)
    expect_equal(nrow(edges_df), 3)

    # there are two in and one out edges (all subclass_of)
    subtypes <- g %>% fetch_edges(direction = "in")

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 3)
    expect_equal(nrow(edges_df), 2)

    # there are two in and one out edges (all subclass_of)
    subtypes <- g %>% fetch_edges(direction = "out")

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_equal(nrow(nodes_df), 2)
    expect_equal(nrow(edges_df), 1)

    # 8 outgoing biolink:has_phenotype to biolink:PhenotypicFeature
    # 1 incoming biolink:causes from biolink:Gene
    # 1 outgoing biolink:subclass_of to biolink:Disease
    # 1 outgoing biolink:has_mode_of_inheritance to biolink:PhenotypicFeature
    g <- query_ids("MONDO:0012187")
    phenos <- g %>% fetch_edges(predicates = "biolink:has_phenotype",
                                result_categories = "biolink:PhenotypicFeature")
    expect_equal(phenos %>% activate(edges) %>% data.frame() %>% nrow(), 8)

    ancestors <- g %>% fetch_edges(direction = "out", predicates = "biolink:subclass_of")
    expect_equal(ancestors %>% activate(edges) %>% data.frame() %>% nrow(), 1)

    # subclass or mode of inheritance
    outs <- g %>% fetch_edges(direction = "out", predicates = c("biolink:subclass_of", "biolink:has_mode_of_inheritance"))
    expect_equal(outs %>% activate(edges) %>% data.frame() %>% nrow(), 2)

    # should be 9 phenotypic feature result nodes
    phenos <- g %>% fetch_edges(result_categories = "biolink:PhenotypicFeature")
    expect_equal(phenos %>% activate(nodes) %>% data.frame() %>% nrow(), 10)

    # whole neighborhood should be 11 nodes
    neighborhood <- g %>% fetch_edges()
    expect_equal(neighborhood %>% activate(nodes) %>% data.frame() %>% nrow(), 12)

    # there should be 57 phenotypic features connected to this disease and its 2 subtypes
    # check in neo4j: MATCH (p0)<-[r0:`biolink:has_phenotype`]-(n)<-[r:`biolink:subclass_of`*]-(q)-[r2:`biolink:has_phenotype`]-(p)  WHERE n.id IN ["MONDO:0009242"]  RETURN p0, r0, n, r, q, r2, p
    g <- query_ids("MONDO:0009242")
    with_subtypes <- g %>% fetch_edges(direction = "in", predicates = "biolink:subclass_of", transitive = TRUE)
    expect_equal(with_subtypes %>% activate(nodes) %>% data.frame() %>% nrow(), 3)

    phenos <- with_subtypes %>% fetch_edges(predicates = "biolink:has_phenotype",
                                            result_categories = "biolink:PhenotypicFeature")

    phenos_only <- phenos %>% activate(nodes) %>% data.frame() %>% rowwise() %>% filter("biolink:PhenotypicFeature" %in% category)
    expect_equal(phenos_only %>% nrow(), 57)
})