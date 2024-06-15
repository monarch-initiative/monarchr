library(testthat)
library(assertthat)

test_that("get_engine works", {
    # skip for now
    #testthat::skip("temporary skip")
    
    g <- monarch_engine() %>% search_nodes("fanconi anemia", limit = 5)
    engine <- get_engine(g)

    expect_s3_class(engine, "neo4j_engine")
})