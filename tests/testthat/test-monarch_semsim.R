library(testthat)
library(assertthat)

test_that("monarch_semsim works", {
    g1 <- monarch_engine() |> 
       fetch_nodes(query_ids = "MONDO:0007947") |> # Marfan syndrome
       expand(result_categories = "biolink:PhenotypicFeature", limit = 5)


    # MONDO:0007522 EDS classic type
    g2 <- monarch_engine() |> 
       fetch_nodes(query_ids = "MONDO:0007522") |> # EDS classic type
       expand(result_categories = "biolink:PhenotypicFeature", limit = 5)

    sim <- monarch_semsim(g1, g2, metric = "jaccard_similarity")
})