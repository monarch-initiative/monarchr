#' @export
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
expand.neo4j_engine <- function(engine,
																		 graph,
                                        direction = "both",
                                        predicates = NULL,
                                        result_categories = NULL,
                                        transitive = FALSE,
                                        drop_unused_query_nodes = FALSE) {
    assert_that(is.tbl_graph(graph))
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
            g2 <- expand(graph,
                              direction = direction,
                              predicates = predicate,
                              result_categories = result_categories,
                              transitive = transitive,
                              drop_unused_query_nodes = TRUE)
            suppressMessages(graph <- tidygraph::graph_join(graph, g2), classes = "message") # suppress joining info
        }
    }

    node_ids <- as.character(tidygraph::as_tibble(tidygraph::activate(graph, nodes))$id)

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

    result <- cypher_query(engine, query, parameters = list(nodes = node_ids,
                                                    predicates = predicates,
                                                    result_categories = result_categories))

    prefs <- engine$preferences

    result <- result %>%
        tidygraph::activate(nodes) %>%
        mutate(pcategory = normalize_categories(category, prefs$category_priority))

    # if drop_unused_query_nodes is FALSE, we'll keep them by
    # joining the result with the original graph
    if(!drop_unused_query_nodes) {
        suppressMessages(result <- tidygraph::graph_join(graph, result), classes = "message") # suppress joining info
    }

    attr(result, "last_engine") <- engine
    return(result)
}
