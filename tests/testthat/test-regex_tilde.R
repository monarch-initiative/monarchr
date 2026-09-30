library(testthat)
library(assertthat)

test_that("%~% matches regular expressions", {
    expect_equal(c("Tall stature", "Short stature", "Obesity") %~% "stature$", c(TRUE, TRUE, FALSE))
    expect_equal("MONDO:0007525" %~% "^HP:", FALSE)

    # usable in filter() on node tables
    res <- toy_graph() |>
        activate(nodes) |>
        filter(name %~% "^[ab]$") |>
        nodes()
    expect_equal(res$id, c("A", "B"))
})
