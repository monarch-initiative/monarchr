library(testthat)
library(assertthat)

test_that("neo4j_engine caching", {
  e <- neo4j_engine(url = c("http://neo4j.monarchinitiative.org:7474",
  													"http://neo4j.monarchinitiative.org",
  													"https://neo4j.monarchinitiative.org",
  													"http://no.such.url"),
  									cache = TRUE)

  		start_time <- Sys.time()

      g <- fetch_nodes(e, query_ids = "MONDO:0020066")

      test <- g %>% expand(direction = "in",
                           predicates = "biolink:subclass_of",
                           transitive = TRUE)

      end_time <- Sys.time()
      no_cache_elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

      start_time <- Sys.time()

    	test <- g %>% expand(direction = "in",
    										 predicates = "biolink:subclass_of",
    										 transitive = TRUE)

      end_time <- Sys.time()
      cache_elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

      # the cached version should be at least 10x faster
      expect_true(cache_elapsed_time * 10 < no_cache_elapsed_time)
})


test_that("neo4j_engine works as expected (using monarch neo4j db)", {
    #testthat::skip("temporary skip")

    # one of these should work
    e <- neo4j_engine(url = c("http://neo4j.monarchinitiative.org",
    													"https://neo4j.monarchinitiative.org",
                              "http://no.such.url",
                              "http://neo4j.monarchinitiative.org:7474"))

    g <- fetch_nodes(e, query_ids = "MONDO:0006043")
    # this should have 6 subtypes (two direct, four under one of the direct children)
    subtypes <- g %>% expand(direction = "in",
                                  predicates = "biolink:subclass_of",
                                  transitive = TRUE)

    nodes_df <- subtypes %>% activate(nodes) %>% as.data.frame()
    edges_df <- subtypes %>% activate(edges) %>% as.data.frame()
    expect_contains(7 + -2:2, nrow(nodes_df))
    expect_contains(6 + -2:2, nrow(edges_df))

    # there should be a pcategory col of type character
    expect_true("pcategory" %in% names(nodes_df))
    expect_true(is.character(nodes_df$pcategory))

    # there should be a category col of type list
    expect_true("category" %in% names(nodes_df))
    expect_true(is.list(nodes_df$category))
})
