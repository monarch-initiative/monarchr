
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood_nodes_neo4j_engine <- function(graph, engine, direction = "both", ...) {
    # returns a dataframe of node categories and counts
    # operates over the combined neighborhoods of the nodes in g
    if (direction == "both") {
    query <- "MATCH (n)-[r]-(m) WHERE n.id IN $nodes AND NOT m.id IN $nodes RETURN m.category AS category, count(*) AS count"
    } else if (direction == "in") {
    query <- "MATCH (n)<-[r]-(m) WHERE n.id IN $nodes AND NOT m.id IN $nodes RETURN m.category AS category, count(*) AS count"
    } else if (direction == "out") {
    query <- "MATCH (n)-[r]->(m) WHERE n.id IN $nodes AND NOT m.id IN $nodes RETURN m.category AS category, count(*) AS count"
    } else {
    stop("Invalid direction parameter")
    }

    nodes <- data.frame(tidygraph::activate(graph, nodes))
    node_ids <- nodes$id
    if(length(node_ids) == 1) {
        node_ids <- list(node_ids)
    }
    result <- cypher_query_df(engine, query, parameters = list(nodes = node_ids))

    prefs <- engine$preferences
    result$pcategory <- normalize_categories(result$category, prefs$category_priority)

    result <- result %>%
        group_by(pcategory) %>%
        summarize(count = sum(count)) %>%
        ungroup() %>%
        arrange(desc(count))

    return(result)
}

#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood_edges_neo4j_engine <- function(graph, engine, direction = "both", ...) {
    # returns a dataframe of relationship types and counts, broken down by pcategory
    # operates over the combined neighborhoods of the nodes in g
    if (direction == "both") {
    query <- "MATCH (n)-[r]-(m) WHERE n.id IN $nodes AND NOT m.id IN $nodes RETURN r.predicate AS predicate, n.category as query_category, m.category AS result_category, count(*) AS count"
    } else if (direction == "in") {
    query <- "MATCH (n)<-[r]-(m) WHERE n.id IN $nodes AND NOT m.id IN $nodes RETURN r.predicate AS predicate, n.category as query_category, m.category AS result_category, count(*) AS count"
    } else if (direction == "out") {
    query <- "MATCH (n)-[r]->(m) WHERE n.id IN $nodes AND NOT m.id IN $nodes RETURN r.predicate AS predicate, n.category as query_category, m.category AS result_category, count(*) AS count"
    } else {
    stop("Invalid direction parameter")
    }

    nodes <- data.frame(tidygraph::activate(graph, nodes))
    node_ids <- nodes$id
    if(length(node_ids) == 1) {
        node_ids <- list(node_ids)
    }

    result <- cypher_query_df(engine, query, parameters = list(nodes = node_ids))

    # create a pcategory column
    prefs <- engine$preferences
    result$query_pcategory <- normalize_categories(result$query_category, prefs$category_priority)
    result$result_pcategory <- normalize_categories(result$result_category, prefs$category_priority)

    result <- result %>%
        group_by(query_pcategory, query_category, predicate, result_pcategory, result_category) %>%
        summarize(count = sum(count)) %>%
        ungroup() %>%
    	  select(count, query_pcategory, predicate, result_pcategory, query_category, result_category) %>%
        arrange(desc(count))

    return(result)
}


######### Public functions #########

#' @importFrom assertthat assert_that
#' @export
summarize_neighborhood.tbl_kgx <- function(graph, engine = NULL, direction = "both", summarize = "edges") {
    assert_that(summarize %in% c("edges", "nodes"))
    assert_that(direction %in% c("both", "in", "out"))

    use_engine <- NULL
    if(is.null(engine)) {
        use_engine <- get_engine(graph, fail_if_missing = FALSE)
    } else {
        use_engine <- engine
    }

    if(is.null(use_engine)) {
        stop("No engine associated with this graph, nor one provided. Unable to proceed.")
    }

    if("neo4j_engine" %in% class(use_engine)) {
        if (summarize == "edges") {
            return(summarize_neighborhood_edges_neo4j_engine(graph, use_engine, direction))
        } else if (summarize == "nodes") {
            return(summarize_neighborhood_nodes_neo4j_engine(graph, use_engine, direction))
        }
    } else if("file_engine" %in% class(use_engine)) {
        stop("Error: summarize_neighborhood not yet implemented for file_engine.")
    } else {
        stop("Error: unknown or incompatible engine.")
    }
}
