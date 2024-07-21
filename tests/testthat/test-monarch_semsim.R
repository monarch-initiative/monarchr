library(testthat)
library(assertthat)

# g1 does not have a disease node, so in the result the target EDS should not be matched by any query
test_that("monarch_semsim works", {
    suppressWarnings({
        g1 <- monarch_engine() |> 
           fetch_nodes(query_ids = "MONDO:0007947") |> # Marfan syndrome
           expand(categories = "biolink:PhenotypicFeature", limit = 5) |>
           activate(nodes) |>
           filter(pcategory == "biolink:PhenotypicFeature") |>
           mutate(source = "g1")

    # MONDO:0007522 EDS classic type
        g2 <- monarch_engine() |> 
           fetch_nodes(query_ids = "MONDO:0007522") |> # EDS classic type
           expand(categories = "biolink:PhenotypicFeature", limit = 5) |>
           activate(nodes) |>
           mutate(source = "g2")
     })

    sim <- monarch_semsim(g1, g2)

    # the number of edges in the result should equal the number of nodes in the query graph
    expect_equal(sim |> activate(edges) |> nrow(), g1 |> activate(nodes) |> nrow())
    # the edges subject should be the same as the query graph nodes
    expect_equal(sim |> activate(edges) |> pull(subject) |> sort(), g1 |> activate(nodes) |> pull(id) |> sort())
    # the "biolink:Disease" node in g2 should not be matched by any query
    expect_equal(sum(edges(sim)$object == "MONDO:0007522"), 0)

    # now lets rerun the query, including the reverse and keeping all nodes
    # using jaccard similarity for flavor
    sim <- monarch_semsim(g1, g2, metric = "jaccard_similarity", include_reverse = TRUE, keep_unmatched = TRUE)

    # all query and target nodes will be subjects exactly once in the resulting edges
    query_target_node_ids <- c(g1 |> activate(nodes) |> pull(id), g2 |> activate(nodes) |> pull(id))
    expect_equal(sim |> activate(edges) |> pull(subject) |> sort(), query_target_node_ids |> sort())

    # the number of edges in the result should equal the number of nodes in the query graph plus the number of nodes in the target graph
    expect_equal(edges(sim) |> nrow(), length(query_target_node_ids))

    # make sure the engine of the result is the same as the query graph
    expect_equal(sim |> get_engine(), g1 |> get_engine())

    # test plot
    #plot(sim |> graph_join(g1) |> graph_join(g2), node_color = paste(source, pcategory))
    #print(sim)
})