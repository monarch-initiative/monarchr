#' @export
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood_edges.neo4j_engine <- function(e, g, direction = "both") {
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

    nodes <- data.frame(tidygraph::activate(g, nodes))
    node_ids <- nodes$id
    if(length(node_ids) == 1) {
        node_ids <- list(node_ids)
    }

    e <- get_engine(g)

    result <- cypher_query_df(e, query, parameters = list(nodes = node_ids))

    # create a pcategory column
    prefs <- e$preferences
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
