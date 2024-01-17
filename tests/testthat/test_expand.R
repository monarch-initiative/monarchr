library(testthat)

test_that("expand expands only from selected nodes", {
    graph <- seed_graph("MONDO:0005148")
    graph <- expand(graph, to_categories = c("Gene", "Phenotype"))
    print(graph)
})