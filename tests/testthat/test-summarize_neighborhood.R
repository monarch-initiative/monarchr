library(testthat)
library(assertthat)

test_that("summarize_neighborhood returns reasonable results", {
    # skip for now
    #testthat::skip("temporary skip")

		g <- monarch_engine() |> fetch_nodes(query_ids = "MONDO:0019391")

    result <- summarize_neighborhood(g, summarize = "edges")
    disease_subclass_of_disease <- result %>% filter(predicate == "biolink:subclass_of" &
                                                     query_pcategory == "biolink:Disease" &
                                                     result_pcategory == "biolink:Disease")

    expect_contains(30 + -4:4, disease_subclass_of_disease$count)
    expect_contains(1 + 0:2, nrow(disease_subclass_of_disease))

    result <- summarize_neighborhood(g, summarize = "nodes")
    disease_nodes <- result %>% filter(pcategory == "biolink:Disease")
    expect_contains(30 + -4:4, disease_nodes$count)
    expect_contains(1 + 0:2, nrow(disease_nodes))
    })
