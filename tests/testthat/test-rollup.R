library(testthat)
library(assertthat)

# in the toy graph, A subclass_of B subclass_of C, with counts A=1, B=2, C=4;
# G (count 8) is not connected by subclass_of

test_that("roll_up aggregates over descendants", {
    res <- toy_graph() |>
        activate(nodes) |>
        mutate(
            up = roll_up(count, fun = sum),
            up_excl = roll_up(count, fun = sum, include_self = FALSE)
        ) |>
        nodes()

    # C's descendants are B and A: 4 + 2 + 1
    expect_equal(res$up, c(1, 3, 7, 8))
    # without self; nodes with no descendants sum over nothing
    expect_equal(res$up_excl, c(0, 1, 3, 0))
})

test_that("roll_down aggregates over ancestors", {
    res <- toy_graph() |>
        activate(nodes) |>
        mutate(down = roll_down(count, fun = sum)) |>
        nodes()

    # A's ancestors are B and C: 1 + 2 + 4
    expect_equal(res$down, c(7, 6, 4, 8))
})

test_that("roll_up returns a list column when results have varying lengths", {
    res <- toy_graph() |>
        activate(nodes) |>
        mutate(up = roll_up(id)) |>
        nodes()

    expect_type(res$up, "list")
    expect_setequal(res$up[[3]], c("A", "B", "C"))
    expect_equal(res$up[[4]], "G")
})

test_that("roll requires a valid direction", {
    expect_error(
        toy_graph() |>
            activate(nodes) |>
            mutate(x = roll(count, direction = "sideways")),
        "direction"
    )
})
