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
    
    # let's assume transitive will be FALSE for now, since that is more complicated
    # if(transitive && length(predicates) == 0) {
    #     stop("Transitive closure requires specified predicates.")
    # }

    # if(transitive && length(predicates) > 1) {
    #     # we call recusively on each predicate
    #     for(predicate in predicates) {
    #         g2 <- fetch_edges(g,
    #                           direction = direction,
    #                           predicates = predicate,
    #                           result_categories = result_categories,
    #                           transitive = transitive,
    #                           drop_unused_query_nodes = TRUE)
    #         g <- tidygraph::graph_join(g, g2)
    #     }
    # }

    # ids of nodes in the query graph
    node_ids <- as.character(tidygraph::as_tibble(tidygraph::activate(g, nodes))$id)

    engine_graph <- engine$graph
    if(direction == "out" | direction == "both") {
        result <- engine_graph %>%
            activate(nodes) %>%
            
    }

}