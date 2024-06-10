library(testthat)
library(assertthat)
library(tidyr)

test_that("fetch_edges file_engine works with transitive", {
    #testthat::skip("temporary skip")
    options(width = 150)

    filename <- system.file("extdata", "test/mondo_kgx_tsv-test-10JUNE2024.tar.gz", package = "monarchr")
    e <- file_engine(filename)
    #query_ids = c("MONDO:0007525")
    #query_ids = c("HGNC:2200")
    query_ids = c("MONDO:0007525", "MONDO:0007524", "SO:0000110") # SO:0000110 "sequence_feature" has no subclass_of in the test data

    #g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(transitive = TRUE, predicate = "biolink:subclass_of")
    #g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(transitive = TRUE, predicate = "biolink:subclass_of", result_categories = "biolink:Disease")
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(transitive = TRUE, predicate = "biolink:subclass_of", direction = "out")
    #g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(transitive = TRUE, predicate = "biolink:subclass_of", direction = "out", drop_unused_query_nodes = TRUE, result_categories = "biolink:Disease")
    print(g %>% activate(nodes) %>% as_tibble() %>% select(id, name, pcategory, category, depth), n = 30)
    print(g %>% activate(edges) %>% as_tibble() %>% select(subject, predicate, object), n = 30)
})

test_that("fetch_Edges file_engine works", {
    testthat::skip("temporary skip")
    options(width = 150)

    filename <- system.file("extdata", "test/mondo_kgx_tsv-test-10JUNE2024.tar.gz", package = "monarchr")
    e <- file_engine(filename)
    query_ids = c("MONDO:0007525", "MONDO:0007524", "MONDO:0007523")

    ## Neighborhood of these nodes:


    # engine_graph <- e$graph
    # connected_edges <- engine_graph %>%
    #     activate(edges) %>%
    #     filter(subject %in% query_ids | object %in% query_ids)
    
    # edge_nodes <- connected_edges %>% as_tibble() %>% select(subject, object) %>% gather()
    
    # connected_edges <- connected_edges %>%
    #     activate(nodes) %>%
    #     filter(id %in% edge_nodes$value)

    # print(connected_edges)
    # print(connected_edges %>% activate(nodes) %>% as_tibble() %>% select(id, name, pcategory, category), n = 20)
    # print(connected_edges %>% activate(edges) %>% as_tibble() %>% select(subject, predicate, object), n = 20)

    # return(NULL)

    # # A tibble: 10 x 4
    #    id            name                                                     pcategory                 category 
    #    <chr>         <chr>                                                    <chr>                     <list>   
    #  1 MONDO:0007524 autosomal dominant Ehlers-Danlos syndrome, vascular type biolink:Disease           <chr [1]>
    #  2 MONDO:0007525 Ehlers-Danlos syndrome, arthrochalasia type              biolink:Disease           <chr [1]>
    #  3 MONDO:0007523 Ehlers-Danlos syndrome, hypermobility type               biolink:Disease           <chr [1]>
    #  4 MONDO:0017314 Ehlers-Danlos syndrome, vascular type                    biolink:Disease           <chr [1]>
    #  5 MONDO:0000426 autosomal dominant disease                               biolink:Disease           <chr [1]>
    #  6 HP:0000006    Autosomal dominant inheritance                           biolink:PhenotypicFeature <chr [1]>
    #  7 MONDO:0020522 Ehlers-Danlos syndrome type 7B                           biolink:Disease           <chr [1]>
    #  8 MONDO:0020521 Ehlers-Danlos syndrome type 7A                           biolink:Disease           <chr [1]>
    #  9 HGNC:2201     COL3A1                                                   biolink:Gene              <chr [2]>
    # 10 MONDO:0020066 Ehlers-Danlos syndrome                                   biolink:Disease           <chr [1]>
    # # A tibble: 9 x 3
    #   subject       predicate           object       
    #   <chr>         <chr>               <chr>        
    # 1 MONDO:0007524 biolink:subclass_of MONDO:0000426
    # 2 MONDO:0007524 biolink:subclass_of MONDO:0017314
    # 3 MONDO:0007524 biolink:related_to  HGNC:2201    
    # 4 MONDO:0007524 biolink:related_to  HP:0000006   
    # 5 MONDO:0007525 biolink:subclass_of MONDO:0020066
    # 6 MONDO:0007523 biolink:subclass_of MONDO:0020066
    # 7 MONDO:0007523 biolink:related_to  HGNC:2201    
    # 8 MONDO:0020522 biolink:subclass_of MONDO:0007525
    # 9 MONDO:0020521 biolink:subclass_of MONDO:0007525
    # ✔ |      1   0 | fetch_edges.file_engine              


    ##### Check basic OUT
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(direction = "out")
    # this result should have: 8 nodes; 6 of which have pcategory of biolink:Disease, 1 biolink:PhenotypicFeature, 1 biolink:Gene
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 8)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 6)
    expect_equal(sum(nodes_df$pcategory == "biolink:PhenotypicFeature"), 1)
    expect_equal(sum(nodes_df$pcategory == "biolink:Gene"), 1)

    # Amongst the edges, there should be 4 biolink:subclass_of, and 3 biolink:related_to
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 7)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 4)
    expect_equal(sum(edges_df$predicate == "biolink:related_to"), 3)

    ##### Check basic IN
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(direction = "in")
    # this result should have 5 nodes all of type biolink:Disease (3 original plus 2 new)
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 5)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 5)

    # it should have two edges of type biolink:subclass_of
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 2)

    ##### Check drop_unused_query_nodes with IN
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(direction = "in", drop_unused_query_nodes = TRUE)
    # this result should have 3 nodes all of type biolink:Disease (1 plus 2 new; two of the originals are unused)
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 3)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 3)

    # it should have two edges of type biolink:subclass_of
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 2)

    #### Check basic BOTH
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(direction = "both")
    # this result shold have 10 nodes, 8 biolink:Disease, 1 bio:PhenotypicFeature, 1 bio:Gene
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 10)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 8)
    expect_equal(sum(nodes_df$pcategory == "biolink:PhenotypicFeature"), 1)
    expect_equal(sum(nodes_df$pcategory == "biolink:Gene"), 1)

    # it should have 9 edges, 6 subclass_of, 3 related_to
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 9)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 6)
    expect_equal(sum(edges_df$predicate == "biolink:related_to"), 3)

    #### Check drop_unused_query_nodes with BOTH
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(direction = "both", drop_unused_query_nodes = TRUE)
    # all the query nodes are used, so the result should be the same as the basic BOTH
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 10)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 8)
    expect_equal(sum(nodes_df$pcategory == "biolink:PhenotypicFeature"), 1)
    expect_equal(sum(nodes_df$pcategory == "biolink:Gene"), 1)

    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 9)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 6)

    # Check OUT with related_to
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(predicate = "biolink:related_to", direction = "out")
    # this result should have 5 nodes, 3 biolink:Disease, 1 bio:PhenotypicFeature, 1 bio:Gene
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 5)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 3)
    expect_equal(sum(nodes_df$pcategory == "biolink:PhenotypicFeature"), 1)
    expect_equal(sum(nodes_df$pcategory == "biolink:Gene"), 1)

    # it should have 3 edges, all related_to
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 3)
    expect_equal(sum(edges_df$predicate == "biolink:related_to"), 3)

    ##### Check OUT with resulted_to and drop_unused_query_nodes
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(predicate = "biolink:related_to", direction = "out", drop_unused_query_nodes = TRUE)
    # this result should have 4 nodes, 2 biolink:Disease, 1 bio:PhenotypicFeature, 1 bio:Gene
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 4)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 2)
    expect_equal(sum(nodes_df$pcategory == "biolink:PhenotypicFeature"), 1)

    # it should have 3 edges, all related_to
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 3)
    expect_equal(sum(edges_df$predicate == "biolink:related_to"), 3)

    ##### Check OUT with result_categories biolink:Disease
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(result_categories = "biolink:Disease", direction = "out")
    # this result should have 6 nodes, all biolink:Disease
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 6)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 6)

    # there should be 4 edges, all subclass_of
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 4)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 4)

    ##### Check IN with result_categories biolink:Disease
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(result_categories = "biolink:Disease", direction = "in")
    # this result should have 5 nodes, all biolink:Disease
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 5)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 5)

    # there should be 2 edges, all subclass_of
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 2)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 2)

    ##### Check IN with result_categories biolink:Disease and drop_unused_query_nodes
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(result_categories = "biolink:Disease", direction = "in", drop_unused_query_nodes = TRUE)
    # this result should have 3 nodes, all biolink:Disease
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 3)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 3)

    # there should be 2 edges, all subclass_of
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 2)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 2)

    ##### Check BOTH with result_categories biolink:Disease
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(result_categories = "biolink:Disease", direction = "both")
    # this result should have 8 nodes, all biolink:Disease
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 8)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 8)

    # there should be 6 edges, all subclass_of
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 6)
    expect_equal(sum(edges_df$predicate == "biolink:subclass_of"), 6)

    ##### Check OUT with predicates biolink:related_to and result_categories biolink:Gene
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(predicate = "biolink:related_to", result_categories = "biolink:Gene", direction = "out")
    # this result should have 4 node, 3 biolink:Disease, 1 biolink:Gene
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 4)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 3)
    expect_equal(sum(nodes_df$pcategory == "biolink:Gene"), 1)

    # there should be 2 edges, both related_to
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 2)
    expect_equal(sum(edges_df$predicate == "biolink:related_to"), 2)

    ###### Check OUT with predicates biolink:related_to and result_categories biolink:Gene and drop_unused_query_nodes
    g <- fetch_nodes(e, query_ids = query_ids) %>% fetch_edges(predicate = "biolink:related_to", result_categories = "biolink:Gene", direction = "out", drop_unused_query_nodes = TRUE)
    # this result should have 3 node, 2 biolink:Disease, 1 biolink:Gene
    nodes_df <- g %>% activate(nodes) %>% as_tibble()
    expect_equal(nrow(nodes_df), 3)
    expect_equal(sum(nodes_df$pcategory == "biolink:Disease"), 2)
    expect_equal(sum(nodes_df$pcategory == "biolink:Gene"), 1)

    # there should be 2 edges, both related_to
    edges_df <- g %>% activate(edges) %>% as_tibble()
    expect_equal(nrow(edges_df), 2)
    expect_equal(sum(edges_df$predicate == "biolink:related_to"), 2)
})