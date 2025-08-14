
#' Create a knowledge graph engine object for a neo4j database
#'
#' Creates a knowledge graph engine backed by a neo4j database, from a URL and optional username and password. Knowledge graph "engines"
#' are objects that store information about how to connect to a (potentially large) knowledge graph, and can be used to fetch nodes and edges from the database as local
#' graph objects.
#'
#' Engines store preference information specifying how data are fetched and manipulated; for example,
#' while node `category` is multi-valued (nodes may have multiple categories, for example "biolink:Gene" and "biolink:NamedThing"),
#' typically a single category is used to represent the node in a graph, and is returned as the nodes' `pcategory`. A preference list of categories to use for `pcategory` is
#' stored in the engine's preferences. A default set of preferences is stored in the package for use with KGX (BioLink-compatible) graphs (see https://github.com/biolink/kgx/blob/master/specification/kgx-format.md),
#' but these can be overridden by the user.
#'
#' For `neo4j_engine()`s, preferences are also used to set the node properties to search when using `search_nodes()`, defaulting to regex-based searches on id, name, and description. (The `monarch_engine()` is a type
#' of `neo4j_engine()` with the URL set to the Monarch Neo4j instance, and overrides `search_nodes()` to use the Monarch search API, see `monarch_engine()` for details).
#'
#' @param url A character string indicating the URL of the neo4j database. If given a vector, each will be tried in sequence; if a URL times out (see timeout) or fails, the next is tried.
#' @param username A character string indicating the username for the neo4j database (if needed).
#' @param password A character string indicating the password for the neo4j database (if needed).
#' @param preferences A named list of preferences for the engine.
#' @param timeout Number of sections to wait before trying the next url.
#' @param cache Whether to cache query results in memory for the length of the R session.
#' @param ... Additional arguments passed to `neo2R::startGraph()`.
#' @seealso `file_engine()`, `monarch_engine()`
#' @return An object of class `neo4j_engine`
#' @export
#' @examplesIf neo4j_engine_check("https://neo4j.monarchinitiative.org")
#' library(tidygraph)
#' library(dplyr)
#'
#' engine <- neo4j_engine(url = "https://neo4j.monarchinitiative.org")
#' res <- engine |> fetch_nodes(query_ids = c("MONDO:0007522", "MONDO:0007947"))
#' print(res)
#'
#' @importFrom neo2R startGraph
#' @importFrom R.utils withTimeout
#' @importFrom memoise cache_memory
neo4j_engine <- function(url,
                         username = NA,
                         password = NA,
                         preferences = NULL,
												 timeout = 1,
												 cache = TRUE,
                         ...) {

		graph_conn <- NULL
		success <- FALSE

		for (i in seq_along(url)) {
			message(paste0("Trying to connect to ", url[i]))
			tryCatch({
				# we create a test connection with a timeout to see if the URL is ok
				# catch the error below if it times out
				testconn <- startGraph(url[i],
															 username = username,
															 password = password,
															 .opts = list(timeout = timeout),
															 ...)

				# if it connected, we need a new connection without the timeout,
				# because any specified timeout will be applied to future queries with the connection
				# and not be allowed to run for long
				conn <- startGraph(url[i],
													 username = username,
													 password = password,
													 ...)
				if (!is.null(conn)) {
					graph_conn <- conn
					success <- TRUE
				}
			}, error = function(e) {
				message(paste0("Failed to connect to ", url[i], ": ", e$message))
			})

			if (success) {
				message(paste0("Connected to ", url[i]))
				break
			}
		}

    if (!success) {
        stop("Failed to connect to any of the URLs provided.")
    }

    content_check <- check_neo2r_kgx(graph_conn)
    if(!content_check[[1]]) {
    	message <- paste0(c("There appears to be an issue with the graph content or formatting. The following errors are noted:",
    											content_check[[2]]), collapse = "\n   ")
    	stop(message)
    }

    obj <- base_engine(name = "neo4j_engine",
                       preferences = preferences)

    obj$graph_conn <- graph_conn
    obj$url <- url
    obj$username <- username
    if(cache) {
    	obj$cache <- memoise::cache_memory()
    }

    class(obj) <- c("neo4j_engine", class(obj))
    return(obj)
}


#' @noRd
check_neo2r_kgx <- function(graph_conn) {
	errors <- c()
	nodes_ok <- TRUE
	edges_ok <- TRUE
	res <- neo2R::cypher(graph_conn, query = "MATCH (n)-[r]-(m) RETURN n, r, m LIMIT 2", result = "graph")
	if(!"nodes" %in% names(res)) {
		nodes_ok <- FALSE
		errors <- c(errors, "No nodes present. Does the Neo4j graph contain any nodes?")
	}
	else if(length(res[["nodes"]]) < 1) {
		nodes_ok <- FALSE
		errors <- c(errors, "No nodes present. Does the Neo4j graph contain any nodes?")
	}

	if(nodes_ok) {
		node1 <- res[["nodes"]][[1]]
		if(!"id" %in% names(node1$properties)) {
			nodes_ok <- FALSE
			errors <- c(errors, "Found a node with no `id` property. Is the Neo4j graph properly KGX formatted? Nodes in KGX formatted graphs in Neo4j must have a character vector `id` property formatted as a CURIE (e.g. 'MONDO:0019391').")
		}
		if(!"category" %in% names(node1[["properties"]])) {
			nodes_ok <- FALSE
			errors <- c(errors, "Found a node with no `category` property. Is the Neo4j graph properly KGX formatted? Nodes in KGX formatted graphs in Neo4j must have a `category` array property (e.g. ['biolink:Entity', 'biolink:NamedThing', 'biolink:Cell']).")
		}
	}

	if(!"relationships" %in% names(res)) {
		edges_ok <- FALSE
		errors <- c(errors, "No relationships present. Does the Neo4j graph contain any edges?")
	}
	else if(length(res[["relationships"]]) < 1) {
		edges_ok <- FALSE
		errors <- c(errors, "No relationships present. Does the Neo4j graph contain any edges?")
	}

	if(edges_ok) {
		edge1 <- res[["relationships"]][[1]]
		if(!"subject" %in% names(edge1[["properties"]])) {
			edges_ok <- FALSE
			errors <- c(errors, "Found a relationship with no `subject` property. Is the Neo4j graph properly KGX formatted? Edges in KGX formatted graphs in Neo4j must have a `subject` property (e.g. 'MONDO:0019391').")
		}
		if(!"predicate" %in% names(edge1[["properties"]])) {
			edges_ok <- FALSE
			errors <- c(errors, "Found a relationship with no `predicate` property. Is the Neo4j graph properly KGX formatted? Edges in KGX formatted graphs in Neo4j must have a `predicate` property (e.g. 'biolink:has_phenotype').")
		}
		if(!"object" %in% names(edge1[["properties"]])) {
			edges_ok <- FALSE
			errors <- c(errors, "Found a relationship with no `object` property. Is the Neo4j graph properly KGX formatted? Edges in KGX formatted graphs in Neo4j must have a `object` property (e.g. 'HP:0004322').")
		}
	}
	ok <- all(nodes_ok & edges_ok)

	return(list(ok, errors))
}
