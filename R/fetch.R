#' Expand a graph by fetching connected edges from the KG.
#'
#' @param g A graph
#' @param direction The direction of associations to fetch. Can be "in", "out", or "both". Default is "both".
#' @param predicates A vector of relationship predicates (nodes in g are subjects in the KG), indicating which edges to consider in the neighborhood. If NULL (default), all edges are considered.
#' @param result_categories A vector of node categories, indicating which nodes in the larger KG (objects in the relationship) to consider as potential result nodes. If NULL (default), all object nodes in the larger KG are considered as potential nodes.
#' @param transitive NOT IMPLEMENTED If TRUE, include transitive closure of the neighborhood. Default is FALSE. Useful in combination with predicates like `biolink:subclass_of`.
#' @param drop_unused_query_nodes If TRUE, remove query nodes from the result, unless they are at the neighboorhood boundary, ie, required for connecting to the result nodes. Default is FALSE.
#'
#' @return A tbl_kgx graph
#' @export
#' @examples
#' g <- monarch_search("fanconi anemia", limit = 1)
#' phenos <- g %>%
#'   fetch_edges(predicates = "biolink:has_phenotype", result_categories = "biolink:PhenotypicFeature")
#'
#' ancestors <- g %>%
#'  fetch_edges(predicates = "biolink:subclass_of", transitive = TRUE)
#'
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
fetch_edges <- function(g,
                        direction = "both",
                        predicates = NULL,
                        result_categories = NULL,
                        transitive = FALSE) {
    assert_that(is.tbl_graph(g))
    assert_that(is.null(predicates) | is.character(predicates))
    assert_that(is.null(result_categories) | is.character(result_categories))
    assert_that(is.logical(transitive))
    if(transitive) {
        stop("Transitive closure not yet implemented")
    }

    node_ids <- as.character(tidygraph::as_tibble(tidygraph::activate(g, nodes))$id)

    # neo4j queries that use IN require a list of values, and a length-1 character vector is treated as a scalar
    if (length(node_ids) == 1) {
        node_ids <- list(node_ids)
    }
    # do the same length checks for predicates and result_categories
    if (!is.null(predicates)) {
        if (length(predicates) == 1) {
            predicates <- list(predicates)
        }
    }
    if (!is.null(result_categories)) {
        if (length(result_categories) == 1) {
            result_categories <- list(result_categories)
        }
    }


    if(direction == "in") {
        query <- "MATCH (n)<-[r]-(m) WHERE n.id IN $nodes"
    } else if(direction == "out") {
        query <- "MATCH (n)-[r]->(m) WHERE n.id IN $nodes"
    } else if(direction == "both") {
        query <- "MATCH (n)-[r]-(m) WHERE n.id IN $nodes"
    } else {
        stop("Invalid direction")
    }

    parameters <- list(nodes = node_ids)
    if (!is.null(predicates)) {
       query <- paste0(query, " AND r.predicate IN $predicates")
       parameters$predicates <- predicates
    }

    if (!is.null(result_categories)) {
       # remember, m.category is an array of strings
       query <- paste0(query, " AND ANY(category IN m.category WHERE category IN $result_categories)")
       parameters$result_categories <- result_categories
    }

    query <- paste0(query, " RETURN n, r, m")

    result <- cypher_query(query, parameters = list(nodes = node_ids,
                                                       predicates = predicates,
                                                       result_categories = result_categories))

    result <- result %>%
        tidygraph::activate(nodes) %>%
        mutate(pcategory = normalize_categories(category, kg_prefs$monarch_kg$category_priority))

    # # if drop_unused_query_nodes is FALSE, we'll keep them by
    # # joining the result with the original graph
    # if(!drop_unused_query_nodes) {
    #     # we'll join on id and all column names appearing in both graphs node data
    #     query_node_cols <- colnames(tidygraph::as_tibble(tidygraph::activate(g, nodes)))
    #     result_node_cols <- colnames(tidygraph::as_tibble(tidygraph::activate(result, nodes)))
    #     common_cols <- intersect(query_node_cols, result_node_cols)

    #     result <- tidygraph::graph_join(g, result, by = common_cols)
    # }

    return(result)
}
