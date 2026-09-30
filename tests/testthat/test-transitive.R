library(testthat)
library(assertthat)

test_that("transitive_closure adds implied edges", {
    g <- toy_graph()
    closed <- transitive_closure(g, predicate = "biolink:subclass_of")

    # A -> B -> C implies A -> C; existing edges are kept, not duplicated
    expect_equal(edge_keys(closed), sort(c(
        "A biolink:subclass_of B",
        "B biolink:subclass_of C",
        "A biolink:subclass_of C",
        "G biolink:causes A"
    )))

    # the new edge is marked as transitive
    new_edge <- edges(closed) |> filter(subject == "A", object == "C")
    expect_equal(new_edge$primary_knowledge_source, "transitive_biolink:subclass_of")

    # the active table is preserved
    expect_equal(active(closed), active(g))
})

test_that("transitive_closure returns input if no edges have the predicate", {
    g <- toy_graph()
    expect_equal(edge_keys(transitive_closure(g, predicate = "biolink:nope")), edge_keys(g))
})

test_that("transitive_closure requires a single predicate", {
    expect_error(transitive_closure(toy_graph(), predicate = c("a", "b")), "length 1")
})

test_that("transitive_reduction removes implied edges", {
    skip_if_not_installed("sets")
    skip_if_not_installed("relations")

    g <- toy_graph()
    closed <- transitive_closure(g, predicate = "biolink:subclass_of")
    reduced <- transitive_reduction(closed, predicate = "biolink:subclass_of")

    # reducing the closure gets us back to the original edges
    expect_equal(edge_keys(reduced), edge_keys(g))
})

test_that("transitive_reduction only reduces the given predicate (#90)", {
    skip_if_not_installed("sets")
    skip_if_not_installed("relations")

    # A -> B -> C by subclass_of, plus an A -> C causes edge that is "implied"
    # by the subclass_of path but has a different predicate
    nodes <- data.frame(id = c("A", "B", "C"))
    nodes$category <- list("x", "x", "x")
    edges <- data.frame(
        subject = c("A", "B", "A", "A"),
        predicate = c("biolink:subclass_of", "biolink:subclass_of", "biolink:subclass_of", "biolink:causes"),
        object = c("B", "C", "C", "C")
    )
    g <- tbl_kgx(nodes = nodes, edges = edges)

    reduced <- transitive_reduction(g, predicate = "biolink:subclass_of")

    # the redundant A subclass_of C edge is removed; the causes edge is kept
    expect_equal(edge_keys(reduced), sort(c(
        "A biolink:subclass_of B",
        "B biolink:subclass_of C",
        "A biolink:causes C"
    )))
})

test_that("transitive_reduction works when node order differs from edge order", {
    skip_if_not_installed("sets")
    skip_if_not_installed("relations")

    # unconnected nodes first, and the chain nodes in reverse order, so node
    # indices don't line up with positions in the reduction's incidence matrix
    nodes <- data.frame(id = c("X", "Y", "C", "B", "A"))
    nodes$category <- list("x", "x", "x", "x", "x")
    edges <- data.frame(
        subject = c("A", "B", "A"),
        predicate = "biolink:subclass_of",
        object = c("B", "C", "C")
    )
    g <- tbl_kgx(nodes = nodes, edges = edges)

    reduced <- transitive_reduction(g, predicate = "biolink:subclass_of")
    expect_equal(edge_keys(reduced), sort(c(
        "A biolink:subclass_of B",
        "B biolink:subclass_of C"
    )))
    expect_equal(sort(nodes(reduced)$id), sort(nodes(g)$id))
})

test_that("transitive_reduction returns input if no edges have the predicate", {
    skip_if_not_installed("sets")
    skip_if_not_installed("relations")

    g <- toy_graph()
    expect_equal(edge_keys(transitive_reduction(g, predicate = "biolink:nope")), edge_keys(g))
})
