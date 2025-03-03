########### Private functions ###########


#' Stitch Vectors
#'
#' @description Neo4j array results come back as lists of length-1 character vectors;
#' these need to be stitched together to length-N character vectors.
#' We leave these in a list element to distinguish sets of size 1 from
#' scalar strings
#'
#' @param x An element to be stitched.
#' @return A list of single-character vectors.
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

#' Remove names from columns
#'
#' Given a data-frame like object, runs each column through unname()
#'
#' @param df Input data frame
#' @importFrom rlang is_named
#' @return The input, with unnamed columns
unname_cols <- function(df) {
	for(name in names(df)) {
		df[[name]] <- unname(df[[name]])
	}

	return(df)
}

#' Process neo2R cypher to tbl_kgx
#'
#' Given a result from neo2R::cypher returning KGX-formatted nodes and edges,
#' parse the result to generate a tbl_kgx object, attaching the provided engine.
#'
#' @param res The result from neo2R::cypher with result = "graph"
#' @param engine The engine to attach to the returned graph
#' @importFrom memoise memoise
#' @importFrom rlang is_named
#' @return A tbl_kgx
neo2r_to_kgx <- function(res, engine) {
	relationship_ids_contained <- as.integer(unlist(res$paths))

	if(!is.null(res)) {
		res <- stitch_vectors(res)
	}

	## node info
	node_ids <- unlist(lapply(res$nodes, function(node) {
		node$properties$id
	}))

	node_categories <- lapply(res$nodes, function(node) {
		node$properties$category[[1]] ## pull these out of the list container to get a simple list of vecs
	})

	nodes_df <- tibble::tibble(id = node_ids, category = node_categories)

	## compute a pcategory, or priority category, based on a preference list
	prefs <- engine$preferences
	nodes_df$pcategory <- normalize_categories(node_categories, prefs$category_priority)

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

	nodes_df <- unname_cols(nodes_df)

	if(is.null(res$relationships[[1]])) {
		g <- tbl_kgx(nodes_df, attach_engine = engine)
		return(g)
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
		edges_df[[prop_name]] <- sapply(res$relationships, function(edge) {
			#				edge$properties[[prop_name]]
			prop_value <- edge$properties[[prop_name]]
			if(is.null(prop_value)) {
				return(NA)
			} else {
				return(prop_value)
			}
		})
	}

	# set from and to info for graph
	edges_df$from <- edge_subjects
	edges_df$to <- edge_objects

	edges_df <- unname_cols(edges_df)

	g <- tbl_kgx(nodes_df, edges_df, attach_engine = engine)
	attr(g, "relationship_ids") <- relationship_ids_contained
	return(g)
}

internal_cypher_query <- function(engine, query, parameters = NULL, ...) {	#
	if(length(query) == 1) {
		res <- neo2R::cypher(engine$graph_conn, query = query, parameters = parameters, result = "graph")
		# NB: this will be NULL if there are no matches.
		return(neo2r_to_kgx(res, engine = engine))
	} else {
		res <- neo2R::multicypher(engine$graph_conn, queries = query, parameters = parameters, result = "graph")
		graphs <- lapply(res, neo2r_to_kgx, engine = engine)
		g <- tbl_kgx(nodes = data.frame())
		for(g2 in graphs) {
			suppressMessages(g <- tidygraph::graph_join(g, g2), classes = "message") # suppress joining info
		}
		return(g)
	}
}

#internal_cypher_query_memoised <- memoise::memoise(internal_cypher_query)

########### Public functions ###########

#' @export
#' @importFrom neo2R cypher
#' @importFrom neo2R multicypher
#' @importFrom tibble tibble
#' @importFrom tidygraph graph_join
cypher_query.neo4j_engine <- function(engine, query, parameters = NULL, ...) {	#

	if(!is.null(engine$cache)) {
		# ok, this is a bit wonky
		# the engine stores its cache
		# we create a memoized internal function using that cache
		# and then we call the function
		# BUT, the engine itself needs to be sent to the function,
		# and if its cache keeps changing it wont memoize properly
		# so we create a copy of the engine without a cache and use that
		engine_copy <- engine
		engine_copy$cache <- NULL

		internal <- memoise::memoise(internal_cypher_query, cache = engine$cache)
		res <- internal(engine_copy, query, parameters, ...)

		# before we return, we reset the cache of the engine attached to the graph.
		res$engine$cache <- engine$cache
		return(res)
	} else {
		internal_cypher_query(engine, query, parameters, ...)
	}

}
