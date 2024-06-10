fetch_edges.file_engine <- function(engine,
                                    g,
                                    direction = "both",
                                    predicates = NULL,
                                    result_categories = NULL,
                                    transitive = FALSE,
                                    drop_unused_query_nodes = FALSE) {

    assert_that(is.tbl_graph(g))
    assert_that(direction %in% c("in", "out", "both"))
    assert_that(is.null(predicates) | is.character(predicates))
    assert_that(is.null(result_categories) | is.character(result_categories))
    assert_that(is.logical(transitive))


    if(direction == "out" | direction == "in") { 
        new_edges <- direction_fetch_internal(engine, g, direction, predicates, result_categories, drop_unused_query_nodes)
        return(new_edges)
        # print(new_out_edges %>% activate(nodes) %>% as_tibble() %>% select(id, name, pcategory, category), n = 20)
        # print(new_out_edges %>% activate(edges) %>% as_tibble() %>% select(subject, predicate, object), n = 20)
    } else if(direction == "both") {
        new_out_edges <- direction_fetch_internal(engine, g, "out", predicates, result_categories, drop_unused_query_nodes)
        new_in_edges <- direction_fetch_internal(engine, g, "in", predicates, result_categories, drop_unused_query_nodes)
        new_edges <- graph_join(new_out_edges, new_in_edges)
        return(new_edges)
    }


    return(NULL)

}

# supports non-transitive, out or in only
direction_fetch_internal <- function(engine,
                                     g,
                                     direction = "out",
                                     predicates = NULL,
                                     result_categories = NULL,
                                     drop_unused_query_nodes = FALSE) {

    engine_graph <- engine$graph

    # ids of nodes in the query graph
    node_ids <- as.character(tidygraph::as_tibble(tidygraph::activate(g, nodes))$id)

    # get outgoing edges from the query nodes (keeps all nodes in the engine graph)
    if(direction == "out") {
        new_edges <- engine_graph %>%
            activate(edges) %>%
            filter(subject %in% node_ids)
    } else if(direction == "in") {
        new_edges <- engine_graph %>%
            activate(edges) %>%
            filter(object %in% node_ids)
    } else {
        stop("direction must be 'in' or 'out' for this internal method.")
    }

    # if predicates are specified, filter by them
    if(!is.null(predicates)) {
        new_edges <- new_edges %>%
            filter(predicate %in% predicates)
    }

    # now let's get all the nodes connected to those edges on either end
    new_edges_df <- new_edges %>% as_tibble()
    new_nodes <- c(new_edges_df$object, new_edges_df$subject)

    # now we keep just the nodes in the engine graph that are connected to the kept edges
    # this includes the query nodes and the result nodes
    new_edges <- new_edges %>%
        activate(nodes) %>%
        filter(id %in% new_nodes)

    # print(new_edges %>% activate(nodes) %>% as_tibble() %>% select(id, name, pcategory, category), n = 20)
    # print(new_edges %>% activate(edges) %>% as_tibble() %>% select(subject, predicate, object), n = 20)


    # if result categories is not NULL, we need to further filter the nodes
    # note that node category is a list column, each node can have multiple categories
    # we need to use map_lgl from the purrr package to check if any of the categories are in the result_categories
    # we also need to keep all the query nodes
    if(!is.null(result_categories)) {
        new_edges <- new_edges %>%
            filter(purrr::map_lgl(category, ~ any(.x %in% result_categories)) | id %in% node_ids)
    }

    # the logic above drops unused query nodes, but we can keep them if desired
    # to do so we drop all the edges in the query graph, and join the result with new_edges
    if(!drop_unused_query_nodes) {
        query_no_edges <- g %>%
            activate(edges) %>%
            filter(FALSE)

        new_edges <- graph_join(query_no_edges, new_edges)
    }

    return(new_edges)
}