############ Internal functions for fetching edges from a file engine ############

#' @importFrom igraph V
transitive_query_internal <- function(engine,
                                      g,
                                      direction = "out",
                                      predicates = NULL,
                                      categories = NULL) {

    # assert that direction is "out" or "in"
    assert_that(direction == "out" | direction == "in", msg = "Direction must be 'out' or 'in' when using transitive closure.")

    # first let's get the edges that match the predicate
    engine_graph <- engine$graph

    filtered_edges <- engine_graph %>%
        activate(edges) %>%
        filter(predicate %in% predicates)

    # and as usual we'll get the nodes connected to those edges
    filtered_edges_df <- filtered_edges %>% activate(edges) %>% as_tibble()
    new_nodes <- c(filtered_edges_df$object, filtered_edges_df$subject)

    # now we keep just those nodes
    filtered_edges <- filtered_edges %>%
        activate(nodes) %>%
        filter(id %in% new_nodes)

    query_ids <- as.character(tidygraph::as_tibble(tidygraph::activate(g, nodes))$id)

    # let's use igraph to get the node identifiers for each node listed in query_ids from the filtered_edges graph
    igraph_ids <- igraph::V(as.igraph(filtered_edges))$id
    igraph_id_nums <- seq_along(igraph_ids)
    names(igraph_id_nums) <- igraph_ids
    query_id_nums <- igraph_id_nums[query_ids]

    # if there are query IDs that are not in the post-filtered graph, they will have NA values, we need to remove them
    query_id_nums <- query_id_nums[!is.na(query_id_nums)]

    # now we can do a breadth-first search from the query nodes
    bfs_result <- filtered_edges %>%
        activate(nodes) %>%
        mutate(depth = bfs_dist(root = query_id_nums, mode = direction)) %>%
        filter(depth >= 0) %>%
        arrange(depth)

    if(!is.null(categories)) {
        bfs_result <- bfs_result %>%
            filter(purrr::map_lgl(category, ~ any(.x %in% categories)) | id %in% query_ids)
    }

    suppressMessages(bfs_result <- kg_join(g, bfs_result), classes = "message")

    attr(bfs_result, "last_engine") <- engine
    return(bfs_result)
}

# supports non-transitive, out or in only
direction_fetch_internal <- function(engine,
                                     g,
                                     direction = "out",
                                     predicates = NULL,
                                     categories = NULL) {

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

    # if result categories is not NULL, we need to further filter the nodes
    # note that node category is a list column, each node can have multiple categories
    # we need to use map_lgl from the purrr package to check if any of the categories are in the categories
    # we also need to keep all the query nodes
    if(!is.null(categories)) {
        new_edges <- new_edges %>%
            filter(purrr::map_lgl(category, ~ any(.x %in% categories)) | id %in% node_ids)
    }

    suppressMessages(new_edges <- kg_join(g, new_edges), classes = "message") # suppress joining info

    return(new_edges)
}



#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
expand_file_engine <- function(engine,
                                    graph,
                                    direction = "both",
                                    predicates = NULL,
                                    categories = NULL,
                                    transitive = FALSE) {

    assert_that(is.tbl_graph(graph))
    assert_that(direction %in% c("in", "out", "both"))
    assert_that(is.null(predicates) | is.character(predicates))
    assert_that(is.null(categories) | is.character(categories))
    assert_that(is.logical(transitive))

    new_edges <- NULL

    if(transitive && length(predicates) != 1) {
      stop("Transitive closure requires exactly one specified predicate.")

    } else if(transitive) {
        new_edges <- transitive_query_internal(engine, graph, direction, predicates, categories)

    } else {
        if(direction == "out" || direction == "in") {
            new_edges <- direction_fetch_internal(engine, graph, direction, predicates, categories)
        } else if(direction == "both") {
            new_out_edges <- direction_fetch_internal(engine, graph, "out", predicates, categories)
            new_in_edges <- direction_fetch_internal(engine, graph, "in", predicates, categories)
            suppressMessages(new_edges <- kg_join(new_out_edges, new_in_edges), classes = "message") # suppress joining info
        }
    }

    return(new_edges)
}

