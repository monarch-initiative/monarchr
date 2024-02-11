library(testthat)
library(assertthat)

test_that("fetch_outgoing works as expected", {
    # g <- monarch_search("DKC1", limit = 1)
    # diseases <- g %>% fetch_outgoing(predicates = "biolink:causes",
    #                                  result_categories = "biolink:Disease",
    #                                  drop_unused_query_nodes = FALSE)

    # print(diseases)

    g <- monarch_search("Fanconi Anemia", limit = 5)
    phenos <- g %>% fetch_outgoing(predicates = "biolink:has_phenotype",
                                   result_categories = "biolink:PhenotypicFeature",
                                   drop_unused_query_nodes = FALSE)

    ## 
    print(phenos %>% activate(edges) %>% as_tibble() %>% pull(object) %>% sort() %>% rle() %>% unique())

    # ancestors <- g %>% fetch_outgoing(predicates = "biolink:subclass_of", 
    #                                   transitive = TRUE)

    # print(ancestors)
})