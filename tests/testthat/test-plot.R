library(testthat)
library(assertthat)

data(eds_marfan_kg)

test_that("plot.tbl_kgx builds a ggraph plot", {
    g <- suppressMessages(eds_marfan_kg |>
        fetch_nodes(query_ids = "MONDO:0007525") |>
        expand_n(predicates = "biolink:subclass_of", direction = "out", n = 2))

    p <- suppressMessages(plot(g))
    expect_s3_class(p, "ggraph")
    # building the plot evaluates all the aesthetics
    expect_no_error(suppressMessages(ggplot2::ggplot_build(p)))

    p_ids <- suppressMessages(plot(g, plot_ids = TRUE, layout = "kk"))
    expect_no_error(ggplot2::ggplot_build(p_ids))
})

test_that("plot.tbl_kgx warns about missing columns", {
    g <- toy_graph()
    expect_warning(suppressMessages(plot(g, edge_linetype = nonexistent)), "not found")
    expect_warning(suppressMessages(plot(g, node_shape = nonexistent)), "not found")
})

test_that("knit_print.tbl_kgx renders HTML tables", {
    out <- knit_print(toy_graph())
    expect_s3_class(out, "knit_asis")
    expect_match(as.character(out), "Graph with 4 nodes and 3 edges")
    expect_match(as.character(out), "Node Data")

    # show limits the number of rows displayed
    out2 <- knit_print(toy_graph(), show = 2)
    expect_match(as.character(out2), "Showing 2 of 4 nodes")
})
