library(testthat)
library(assertthat)

data(eds_marfan_kg)

test_that("expand_n expands n times", {
    g <- eds_marfan_kg |> fetch_nodes(query_ids = "MONDO:0007525")

    each <- suppressMessages(g |> expand_n(
        predicates = "biolink:subclass_of", direction = "out", n = 2, return_each = TRUE
    ))
    expect_equal(names(each), c("iteration0", "iteration1", "iteration2"))

    # each iteration can only grow the graph
    sizes <- vapply(each, function(x) nrow(nodes(x)), integer(1))
    expect_equal(unname(sizes[1]), 1)
    expect_true(all(diff(sizes) >= 0))
    expect_gt(sizes[["iteration2"]], sizes[["iteration1"]])

    # without return_each, the result is the final iteration
    final <- suppressMessages(g |> expand_n(
        predicates = "biolink:subclass_of", direction = "out", n = 2
    ))
    expect_equal(sort(nodes(final)$id), sort(nodes(each$iteration2)$id))
})

test_that("expand_n checks list argument lengths", {
    g <- eds_marfan_kg |> fetch_nodes(query_ids = "MONDO:0007525")
    expect_error(
        suppressMessages(g |> expand_n(predicates = list("biolink:subclass_of"), n = 2)),
        "equal to n"
    )
})

test_that("expand_n warns that transitive is ignored", {
    g <- eds_marfan_kg |> fetch_nodes(query_ids = "MONDO:0007525")
    expect_warning(
        suppressMessages(g |> expand_n(predicates = "biolink:subclass_of", n = 1, transitive = TRUE)),
        "transitive"
    )
})
