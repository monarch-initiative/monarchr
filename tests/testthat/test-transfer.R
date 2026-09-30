library(testthat)
library(assertthat)

test_that("transfer moves node data along and against edges", {
    res <- toy_graph() |>
        activate(nodes) |>
        mutate(
            # G causes A, so A receives G's name going "out" along the edge
            caused_by = transfer(name, over = "biolink:causes", direction = "out"),
            # and G receives A's name going "in" against the edge
            causes = transfer(name, over = "biolink:causes", direction = "in")
        ) |>
        nodes()

    expect_equal(res$caused_by, c("g", NA, NA, NA))
    expect_equal(res$causes, c(NA, NA, NA, "a"))
})

test_that("transfer only uses the given predicates", {
    res <- toy_graph() |>
        activate(nodes) |>
        mutate(parent = transfer(name, over = "biolink:subclass_of", direction = "in")) |>
        nodes()

    # A's parent is B, B's parent is C; the causes edge is ignored
    expect_equal(res$parent, c("b", "c", NA, NA))
})

test_that("transfer requires a valid direction", {
    expect_error(
        toy_graph() |>
            activate(nodes) |>
            mutate(x = transfer(name, over = "biolink:causes", direction = "sideways")),
        "'in' or 'out'"
    )
})
