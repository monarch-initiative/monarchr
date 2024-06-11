#' Normalize Categories
#'
#' This function takes a list of vectors of categories and an ordered preference list over categories.
#' It selects the most preferred category from each vector, or the first category if no preferred categories are included.
#'
#' @param cats_list A list of vectors of categories.
#' @param cats_prefs An ordered preference list over categories.
#' @return A vector of normalized categories.
normalize_categories <- function(cats_list, cats_prefs) {
	normed <- unlist(lapply(cats_list, function(categories) {
		positions <- match(categories, cats_prefs)

		# If all matches are NO, there is no preference match, so use the first label;
		# otherwise, use the label with the minimum position in the preferred list
		most_preferred_label <- if (all(is.na(positions))) {
			categories[1]
		} else {
			categories[which.min(positions)]
		}

		most_preferred_label
	}))

	return(normed)
}

## Neo4j array results come back as lists of length-1 character vectors;
## these need to be stitched together to length-N character vectors.
## We leave these in a list element to distinguish sets of size 1 from
## scalar strings
stitch_vectors <- function(x) {
	# Check if the element is a list
	if (is.list(x)) {
		# Check if all elements of the list are single-character vectors
		if (all(sapply(x, function(y) is.character(y) && length(y) == 1))) {
			# Concatenate all single-character vectors and put them in a list
			return(list(unlist(x)))
		} else {
			# Recursively apply the function to each element of the list
			return(lapply(x, stitch_vectors))
		}
	} else {
		# Return the element as is if it's not a list
		return(x)
	}
}

#' Query Graph
#'
#' This function takes a cypher query and returns a tbl_kgx graph. It retrieves the graph connection and knowledge graph preferences from the parent environment.
#' It then executes the cypher query and stitches the vectors. It creates a data frame for nodes and edges and adds all other node and edge properties as columns.
#' It also computes a priority category based on a preference list. Finally, it creates a tbl_kgx graph from the nodes and edges data frames.
#'
#' @param query A string representing the cypher query.
#' @param parameters Parameters for the cypher query. Default is NULL.
#' @param ... Additional arguments passed to the function (unused).
#' @return A tbl_kgx graph.
#' @examples
#' query <- "MATCH (s) -[p]- (o) return s, p, o LIMIT 2"
#' parameters <- NULL
#' g <- cypher_query(query, parameters)
#' ids <- c("MONDO:0007525", "MONDO:0020066", "MONDO:0034021")
#' query = "MATCH (n) WHERE n.id IN $ids RETURN n"
#' parameters = list(ids = ids)
#' g <- cypher_query(query, parameters)
#' @export
#' @importFrom neo2R cypher
#' @importFrom tibble tibble
cypher_query <- function(query, parameters = NULL, ...) {
	kg_name <- "monarch" # we can parameterize this in the future if desired

	pkg_env <- parent.env(environment())
	graph_connections <- get("graph_connections", envir = pkg_env)

	res <- neo2R::cypher(graph_connections[[kg_name]], query = query, parameters = parameters, result = "graph")
	res <- stitch_vectors(res)

	## node info
	node_ids <- unlist(lapply(res$nodes, function(node) {
		node$properties$id
	}))

	node_categories <- lapply(res$nodes, function(node) {
		node$properties$category[[1]] ## pull these out of the list container to get a simple list of vecs
	})

	nodes_df <- tibble::tibble(id = node_ids, category = node_categories)

	## compute a pcategory, or priority category, based on a preference list
	nodes_df$pcategory <- normalize_categories(node_categories, options("kg_prefs")$kg_prefs$monarch_kg$category_priority)

	## add all other node properties as columns
	node_prop_names <- unname(unique(unlist(lapply(res$nodes, function(node){
		names(node$properties)
	}))))
	node_prop_names <- node_prop_names[!node_prop_names %in% c("id", "category")]

	for(prop_name in node_prop_names) {
		# sapply!
		nodes_df[[prop_name]] <- sapply(res$nodes, function(node) {
			prop_value <- node$properties[[prop_name]]
			if(is.null(prop_value)) {
				return(NA)
			} else {
				return(prop_value)
			}
		})
	}

	if(is.null(res$relationships[[1]])) {
		return(tbl_kgx(nodes_df))
	}

	## edge info
	edge_subjects <- unlist(lapply(res$relationships, function(relationship) {
		relationship$properties$subject
	}))

	edge_predicates <- unlist(lapply(res$relationships, function(relationship) {
		relationship$properties$predicate
	}))

	edge_objects <- unlist(lapply(res$relationships, function(relationship) {
		relationship$properties$object
	}))

	edges_df <- data.frame(subject = edge_subjects,
												 predicate = edge_predicates,
												 object = edge_objects)

	# add all other edge properties as columns
	edge_prop_names <- unname(unique(unlist(lapply(res$relationships, function(edge){
		names(edge$properties)
	}))))
	edge_prop_names <- edge_prop_names[!edge_prop_names %in% c("subject", "predicate", "object")]

	for(prop_name in edge_prop_names) {
		# sapply!
		# edges_df[[prop_name]] <- sapply(res$relationships, function(edge) {
		edges_df[[prop_name]] <- lapply(res$relationships, function(edge) {
				edge$properties[[prop_name]]
		})
	}

	# set from and to info for graph
	edges_df$from <- edge_subjects
	edges_df$to <- edge_objects

	g <- tbl_kgx(nodes_df, edges_df)
	return(g)
}

#' Execute a Cypher Query and Return a Data Frame
#'
#' This function takes a Cypher query and parameters, executes the query on the Monarch knowledge graph, and returns the result as a data frame.
#'
#' @param query A string representing the Cypher query.
#' @param parameters A list of parameters for the Cypher query. Default is an empty list.
#' @param ... Additional arguments passed to the function.
#' @return A data frame containing the result of the Cypher query.
#' @export
#' @examples
#' query <- "MATCH (n) RETURN n LIMIT 10"
#' ids <- c("MONDO:0007525", "MONDO:0020066", "MONDO:0034021")
#' parameters <- list(ids = ids)
#' result <- cypher_query_df(query, parameters)
#' @importFrom neo2R cypher
cypher_query_df <- function(query, parameters = list(), ...) {
    kg_name <- "monarch" # we can parameterize this in the future if desired

	pkg_env <- parent.env(environment())
	graph_connections <- get("graph_connections", envir = pkg_env)
    result <- neo2R::cypher(graph_connections[[kg_name]], query = query, parameters = parameters, result = "row", arraysAsStrings = FALSE)

    return(result)
}
