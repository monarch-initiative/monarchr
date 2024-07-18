library(testthat)
library(assertthat)

test_that("summarize_neighborhood returns reasonable results", {
    # skip for now
    #testthat::skip("temporary skip")

    g <- monarch_search("fanconi anemia", limit = 1)
    # result should be a tbl_kgx with 5 nodes and no edges

    result <- summarize_neighborhood(g, summarize = "edges")
    # there should be a row with count = 30 where predicate = "biolink:subclass_of"
    disease_subclass_of_disease <- result %>% filter(predicate == "biolink:subclass_of" & 
                                                     query_pcategory == "biolink:Disease" &
                                                     result_pcategory == "biolink:Disease")
    expect_equal(disease_subclass_of_disease$count, 30)
    expect_equal(nrow(disease_subclass_of_disease), 1)

    result <- summarize_neighborhood(g, summarize = "nodes")
    # there should be a row with count = 31 where pcategory = "biolink:Disease"
    disease_nodes <- result %>% filter(pcategory == "biolink:Disease")
    expect_equal(disease_nodes$count, 31)
    expect_equal(nrow(disease_nodes), 1)
    })
