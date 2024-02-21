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
                        transitive = FALSE,
                        drop_unused_query_nodes = FALSE) {
    assert_that(is.tbl_graph(g))
    assert_that(direction %in% c("in", "out", "both"))
    assert_that(is.null(predicates) | is.character(predicates))
    assert_that(is.null(result_categories) | is.character(result_categories))
    assert_that(is.logical(transitive))

    if(transitive && length(predicates) == 0) {
        stop("Transitive closure requires specified predicates.")
    }

    if(transitive && length(predicates) > 1) {
        # we call recusively on each predicate
        for(predicate in predicates) {
            g2 <- fetch_edges(g,
                              direction = direction,
                              predicates = predicate,
                              result_categories = result_categories,
                              transitive = transitive,
                              drop_unused_query_nodes = TRUE)
            g <- tidygraph::graph_join(g, g2)
        }
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

    query_pre <- "-"
    query_post <- "->"
    if(direction == "in") {
        query_pre <- "<-"
        query_post <- "-"
    } else if(direction == "both") {
        query_pre <- "-"
        query_post <- "-"
    }

    `%+%` <- function(a, b) {
        paste0(a, b)
    }

    query_r <- "[r]"
    if(transitive) {
        # if transitive is TRUE here, we know that predicates is a length-1 character vector
        query_r <- "[r:`" %+% predicates %+% "`*]"
    } else if(length(predicates) == 1) {
        query_r <- "[r:`" %+% predicates %+% "`]"
    }

    query <- "MATCH (n)" %+% query_pre %+% query_r %+% query_post %+% "(m) WHERE n.id IN $nodes"
    parameters <- list(nodes = node_ids)

    if(length(predicates) > 1) {
       query <- paste0(query, " AND r.predicate IN $predicates")
       parameters$predicates <- predicates
    }

    if(!is.null(result_categories)) {
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
        mutate(pcategory = normalize_categories(category, options("kg_prefs")$kg_prefs$monarch_kg$category_priority))

    # if drop_unused_query_nodes is FALSE, we'll keep them by
    # joining the result with the original graph
    if(!drop_unused_query_nodes) {
        result <- tidygraph::graph_join(g, result)
    }

    return(result)
}

