library(testthat)
library(assertthat)

test_that("graph_centrality adds a node column", {
    g <- suppressMessages(graph_centrality(toy_graph()))
    expect_type(nodes(g)$centrality, "double")
    expect_length(nodes(g)$centrality, 4)

    # C has no outgoing edges, so zero harmonic centrality (out mode)
    expect_equal(unname(nodes(g)$centrality[3]), 0)

    # custom column name
    g2 <- suppressMessages(graph_centrality(toy_graph(), col = "cent"))
    expect_true("cent" %in% colnames(nodes(g2)))
})

test_that("graph_sparsity computes proportion of zeros", {
    # 4 nodes, 3 edges: 13 of 16 adjacency entries are zero
    expect_equal(graph_sparsity(toy_graph()), 13 / 16)
    expect_error(graph_sparsity(1:3), "igraph object or a \\(sparse\\) matrix")
})

test_that("graph_semsim adds an edge column or returns a matrix", {
    skip_if_not_installed("Matrix")

    g <- suppressMessages(graph_semsim(toy_graph()))
    expect_type(edges(g)$similarity, "double")
    expect_length(edges(g)$similarity, 3)

    m <- suppressMessages(graph_semsim(toy_graph(), return_matrix = TRUE))
    expect_s4_class(m, "Matrix")
    expect_equal(dim(m), c(4, 4))
    expect_equal(rownames(m), c("A", "B", "C", "G"))

    dense <- suppressMessages(graph_semsim(toy_graph(), return_matrix = TRUE, sparse = FALSE))
    expect_true(is.matrix(dense))
})

test_that("layout_umap returns coordinates per node", {
    set.seed(42)
    X <- layout_umap(toy_graph())
    expect_equal(dim(X), c(4, 2))
    expect_equal(colnames(X), c("UMAP1", "UMAP2"))
    expect_equal(rownames(X), c("A", "B", "C", "G"))

    X3 <- layout_umap(toy_graph(), use_3d = TRUE, prefix = "U")
    expect_equal(colnames(X3), c("U1", "U2", "U3"))
})
