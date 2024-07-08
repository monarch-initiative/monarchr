##' @export
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
expand_neo4j_engine <- function(engine,
																		 graph,
                                        direction = "both",
                                        predicates = NULL,
                                        result_categories = NULL,
                                        transitive = FALSE,
                                        drop_unused_query_nodes = FALSE,
																				page_size = 1000) {
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

    query <- "MATCH path = (n)" %+% query_pre %+% query_r %+% query_post %+% "(m) WHERE n.id IN $nodes"
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

    if(transitive) {
    	query <- paste0(query, " UNWIND relationships(path) AS r2")
    	query <- paste0(query, " WITH DISTINCT r2 WITH startNode(r2) AS n2, r2, endNode(r2) AS m2")
    } else {
    	query <- paste0(query, " WITH n as n2, r as r2, m as m2")
    }


    ## preflight check - how many results are we going to fetch in total?
		preflight_query <- paste0(query, " WITH DISTINCT id(r2) as relID RETURN COUNT(relID) as total_results")

		preflight_result <- cypher_query_df(engine,
																				preflight_query,
																				parameters = list(nodes = node_ids,
																											    predicates = predicates,
																												  result_categories = result_categories))

		total_results <- preflight_result$total_results

		## ok, now we fetch
		# start with an empty graph
		last_max_relationship_id <- -1
		result_cumulative <- tbl_kgx(nodes = data.frame())

		last_result_size <- -1
		total_edges_fetched <- 0

		while(last_result_size != 0) {
			result_query <- paste0(query, " WHERE id(r2) > $last_max_relationship_id RETURN r2 ORDER BY id(r2) ASC LIMIT $page_size")

	    result <- cypher_query(engine, result_query, parameters = list(nodes = node_ids,
	                                                    predicates = predicates,
	                                                    result_categories = result_categories,
	    																								last_max_relationship_id = last_max_relationship_id,
	    																								page_size = page_size))

			last_result_size <- nrow(edges(result))

			if(last_result_size > 0) {
				total_edges_fetched <- total_edges_fetched + last_result_size
				last_max_relationship_id <- max(attr(result, "relationship_ids"))
				suppressMessages(result_cumulative <- graph_join(result_cumulative, result), class = "message")
				message(paste("Expanding; fetched", total_edges_fetched, "of", total_results, "edges."))
			}
		}


    prefs <- engine$preferences

    result_cumulative <- result_cumulative %>%
        tidygraph::activate(nodes) %>%
        mutate(pcategory = normalize_categories(category, prefs$category_priority))

    # if drop_unused_query_nodes is FALSE, we'll keep them by
    # joining the result with the original graph
    if(!drop_unused_query_nodes) {
        suppressMessages(result_cumulative <- tidygraph::graph_join(graph, result_cumulative), classes = "message") # suppress joining info
    }

    attr(result_cumulative, "last_engine") <- engine
    return(result_cumulative)
}
