
#' Summarize neighborhood edges
#' 
#' Summarizes the relationships in the neighborhood of a given graph. Specifically,
#' this function retrieves counts of different predicates connecting nodes in the given graph to
#' nodes in the larger KG. Counts are broken down by source and target node categories.
#' Note that the number of relationships returned may be larger than the number of nodes they connect to.
#' 
#' @param g A graph
#' @param direction The direction of edges to include in the neighborhood
#' 
#' @return A tbl_kgx graph
#' 
#' @export
#' @examples
#' g <- monarch_search("fanconi anemia")
#' summarize_neighborhood_edges(g)
#' 
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood_edges <- function(g, direction = "both") {
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
    result <- cypher_query_df(query, parameters = list(nodes = node_ids))

    # create a pcategory column
    result$query_category <- normalize_categories(result$query_category, kg_prefs$monarch_kg$category_priority)
    result$result_category <- normalize_categories(result$result_category, kg_prefs$monarch_kg$category_priority)

    result <- result %>%
        group_by(predicate, query_category, result_category) %>%
        summarize(count = sum(count)) %>%
        ungroup() %>%
        arrange(desc(count)) %>%
        select(query_category, predicate, result_category, count)

    return(result)
}

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
#' @export
#' @examples
#' g <- monarch_search("fanconi anemia")
#' summarize_neighborhood_nodes(g)
#' 
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood_nodes <- function(g, direction = "both") {
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
    result <- cypher_query_df(query, parameters = list(nodes = node_ids))

    result$pcategory <- normalize_categories(result$category, kg_prefs$monarch_kg$category_priority)

    result <- result %>%
        group_by(pcategory) %>%
        summarize(count = sum(count)) %>%
        ungroup() %>%
        arrange(desc(count))

    return(result)
}

#' Summarize neighborhood
#' 
#' Summarizes the neighborhood of the nodes of a given graph. Specifically, 
#' letting $N$ be the set of nodes in the given graph, this function retrieves 
#' counts of relationship predicates (or node categories) of nodes connected to 
#' $N$ but not in $N$. This can be useful to examine the scale and scope of a 
#' graph's collective neighborhood in the larger KG.
#' 
#' Note that the number of relationships returned may be larger than the number 
#' of nodes they connect to; use summarize = "edges" to see edge counts between
#' nodes of different categories, and summarize = "nodes" to see counts of connected
#' node categories.
#' 
#' It is also possible to specify the direction of edges to include in the 
#' neighborhood, using the direction parameter. The default is "both", which
#' includes both incoming and outgoing edges.
#' 
#' 
#' @param g A graph
#' @param direction The direction of edges to include in the neighborhood
#' @param summarize Whether to summarize edges, nodes, or both
#' 
#' @return A tbl_kgx graph
#' 
#' @export
#' @examples
#' g <- monarch_search("fanconi anemia")
#' summarize_neighborhood(g, summarize = "edges") # default
#' summarize_neighborhood(g, summarize = "nodes")
#' 
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood <- function(g, direction = "both", summarize = "edges") {
    assert_that(summarize %in% c("edges", "nodes"))
    assert_that(direction %in% c("both", "in", "out"))
    if (summarize == "edges") {
        return(summarize_neighborhood_edges(g, direction))
    } else if (summarize == "nodes") {
        return(summarize_neighborhood_nodes(g, direction))
    }
}
