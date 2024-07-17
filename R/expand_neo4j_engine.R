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
																				page_size = 1000,
																				limit = NULL) {
	  ## Sanity checks
    assert_that(is.tbl_graph(graph))
    assert_that(direction %in% c("in", "out", "both"))
    assert_that(is.null(predicates) | is.character(predicates))
    assert_that(is.null(result_categories) | is.character(result_categories))
    assert_that(is.logical(transitive))

    if(transitive && length(predicates) != 1) {
        stop("Transitive closure requires exactly one specified predicate.")
    }

		## Get and fix query params: node_ids in query, predicates requested, result categories requested

    # neo4j queries that use IN require a list of values, and a length-1 character vector is treated as a scalar
    node_ids <- as.character(tidygraph::as_tibble(tidygraph::activate(graph, nodes))$id)
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

    # utility function, inline paste
    `%+%` <- function(a, b) { paste0(a, b) }

    ## Build the cypher relationship query
    query_pre <- "-"
    query_post <- "->"
    if(direction == "in") {
        query_pre <- "<-"
        query_post <- "-"
    } else if(direction == "both") {
        query_pre <- "-"
        query_post <- "-"
    }

    query_r <- "[r]"
    if(transitive) {
        # if transitive is TRUE here, we know that predicates is a length-1 character vector
        query_r <- "[r:`" %+% predicates %+% "`*]"
    } else if(length(predicates) == 1) {
        query_r <- "[r:`" %+% predicates %+% "`]"
    }

    ## build the query prefix and parameters list
    query <- "MATCH path = (n)" %+% query_pre %+% query_r %+% query_post %+% "(m) WHERE n.id IN $nodes"
    parameters <- list(nodes = node_ids)

    ## append additional conditionals
    if(length(predicates) > 1) {
       query <- paste0(query, " AND r.predicate IN $predicates")
       parameters$predicates <- predicates
    }

    if(!is.null(result_categories)) {
       # remember, m.category is an array of strings
       query <- paste0(query, " AND ANY(category IN m.category WHERE category IN $result_categories)")
       parameters$result_categories <- result_categories
    }

    ## in order to do paging, if the query is transitive things are tricky -
    # we need to get all of the distinct relationships in the result path set, and order by their id()
    # so here we get those relationships, ids, and nodes on either end (for later ordering)
    # if it's not transitive, we can just get the relationships matched directly
    if(transitive) {
    	query <- paste0(query, " UNWIND relationships(path) AS r2")
    	query <- paste0(query, " WITH DISTINCT r2 WITH startNode(r2) AS n2, r2, endNode(r2) AS m2")
    } else {
    	query <- paste0(query, " WITH n as n2, r as r2, m as m2")
    }

    message("Expanding; counting matching edges... ", appendLF = FALSE)

    ## PREFLIGHT QUERY - how many results are we going to fetch in total?
		preflight_query <- paste0(query, " WITH DISTINCT id(r2) as relID RETURN COUNT(relID) as total_results")

		preflight_result <- cypher_query_df(engine,
																				preflight_query,
																				parameters = list(nodes = node_ids,
																											    predicates = predicates,
																												  result_categories = result_categories))

		total_results <- preflight_result$total_results

		message(" total: ", total_results, ".")
		if(!is.null(limit)) {
			if(limit < total_results) {
				warning("Specified limit (", limit, ") is less than total; attempting to fetch ", limit, " arbitrary new edges not present in query.", immediate. = TRUE, call. = FALSE)
				total_results <- limit
			}
		}


		## FULL QUERY - with paging and limiting

		## get a listing of the edges in the query graph for determining new edge pulls
		query_edges_df <- edges(graph)
		query_edges_triples <- paste(query_edges_df$subject, query_edges_df$predicate, query_edges_df$object)

		# initializations before loop
		last_max_relationship_id <- -1
		result_cumulative <- tbl_kgx(nodes = data.frame())

		total_edges_fetched <- 0
		# for keeping track of which new edges have been fetched (that are not in the query graph)
		new_triples_fetched <- character()

		last_result_size <- -1

		while(last_result_size != 0) {
			result_query <- paste0(query, " WHERE id(r2) > $last_max_relationship_id RETURN r2 ORDER BY id(r2) ASC LIMIT $page_size")

	    result <- cypher_query(engine, result_query, parameters = list(nodes = node_ids,
	                                                    predicates = predicates,
	                                                    result_categories = result_categories,
	    																								last_max_relationship_id = last_max_relationship_id,
	    																								page_size = page_size))

	    ## how many edges did we just fetch?
	    result_edges_df <- edges(result)
	    last_result_size <- nrow(result_edges_df)

			if(last_result_size > 0) {
				## main work: keep track of max relationship fetched, update running graph
				last_max_relationship_id <- max(attr(result, "relationship_ids"))
				suppressMessages(result_cumulative <- graph_join(result_cumulative, result), class = "message")
				total_edges_fetched <- total_edges_fetched + last_result_size

				## which new edges (not present in the query graph) did we fetch?
				result_edges_triples <- paste(result_edges_df$subject, result_edges_df$predicate, result_edges_df$object)
				# this is not ideal, could be optimized with a proper hash/set-like data structure
				new_result_edges_triples <- result_edges_triples[!result_edges_triples %in% query_edges_triples]
				# update running pool of new fetched eddges
				new_triples_fetched <- union(new_triples_fetched, new_result_edges_triples)

				message(paste("Expanding; fetched", total_edges_fetched, "of", total_results, "edges."))

				# the number of new triples has likely increased - if there's a limit
				# we should consider whether we are now over it; if so, trim the new result
				# to just <limit> edges and corresponding nodes, and break
				if(!is.null(limit)) {
					if(length(new_triples_fetched) >= limit) {
						keep_triple_ids <- head(new_triples_fetched, n = limit)

						result_cumulative <- result_cumulative %>%
							activate(edges) %>%
							filter(paste(subject, predicate, object) %in% keep_triple_ids)

						keep_node_ids <- c(edges(result_cumulative)$subject, edges(result_cumulative)$object)
						result_cumulative <- result_cumulative %>%
							activate(nodes) %>%
							filter(id %in% keep_node_ids)

						break
					}
				}
			}
		}

		# set pcategory
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
