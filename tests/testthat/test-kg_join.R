library(testthat)
library(assertthat)

test_that("kg_join works", {

    g1 <- tbl_kgx(nodes = tibble(id = c("a", "b", "c"),
                                 category = c("gene", "gene", "disease"),
                                 source = c("g1", "g1", "g1")),
                  edges = tibble(subject = c("a", "b"),
                                 predicate = c("interacts_with", "interacts_with"),
                                 object = c("b", "c")))

    g2 <- tbl_kgx(nodes = tibble(id = c("c", "c", "d"),
                                 category = c("disease", "disease", "gene"),
                                 source = c("g2", "g2", "g2")),
                  edges = tibble(subject = c("c"),
                                 predicate = c("interacts_with"),
                                 object = c("d")))

    res <- kg_join(g1, g2)

    # because of the duplication of c in the nodes, we should have 5 nodes
    # and because of the edge between c and d, we should have 2 edges for that one
    # along with the other three edges for 5 total
    expect_equal(nrow(nodes(res)), 5)
    expect_equal(nrow(edges(res)), 5)
    expect_equal(nrow(edges(res) |> filter(subject == "c")), 2)
    expect_equal(nrow(edges(res) |> filter(subject == "a")), 1)
    expect_equal(nrow(edges(res) |> filter(subject == "b")), 2)
    expect_equal(nrow(edges(res) |> filter(object == "b")), 1)


    res2 <- file_engine(system.file("extdata", "mondo_kgx_tsv.tar.gz", package = "monarchr")) |>
    	        fetch_nodes(query_ids = "MONDO:0007525")

    # there should be no edges and one node
    expect_equal(nrow(nodes(res2)), 1)
    expect_equal(nrow(edges(res2)), 0)


})
