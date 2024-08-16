library(testthat)
library(assertthat)

test_that("get_engine works", {
    g <- monarch_search("fanconi anemia", limit = 5)
    engine <- get_engine(g)
    expect_s3_class(engine, "neo4j_engine")


    filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
    e <- file_engine(filename)
    query_ids = c("MONDO:0007525", "MONDO:0007524")

    g <- fetch_nodes(e, query_ids = query_ids) %>% expand(predicate = "biolink:subclass_of", transitive = TRUE, direction = "out")
    engine <- get_engine(g)
    expect_s3_class(engine, "file_engine")
})
