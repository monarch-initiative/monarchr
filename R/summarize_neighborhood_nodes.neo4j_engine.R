
#' Summarize neighborhood nodes
#'
#' Summarizes the nodes in the neighborhood of a given graph. Specifically,
#' this function retrieves counts of different node categories in the neighborhood of the given graph.
#'
#' @param g A graph
#' @param direction The direction of edges to include in the neighborhood
#'
#' @return A tbl_kgx graph
#'
#' @examples
#' g <- monarch_search("fanconi anemia")
#' summarize_neighborhood_nodes(g)
#'
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood_nodes.neo4j_engine <- function(e, g, direction = "both") {
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

    nodes <- data.frame(tidygraph::activate(g, nodes))
    node_ids <- nodes$id
    if(length(node_ids) == 1) {
        node_ids <- list(node_ids)
    }
    result <- cypher_query_df(e, query, parameters = list(nodes = node_ids))

    prefs <- e$preferences
    result$pcategory <- normalize_categories(result$category, prefs$category_priority)

    result <- result %>%
        group_by(pcategory) %>%
        summarize(count = sum(count)) %>%
        ungroup() %>%
        arrange(desc(count))

    return(result)
}