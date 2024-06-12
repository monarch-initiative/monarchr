#' Check if a neo4j database is available and properly formatted
#' 
#' Attempts to connect to the specified Neo4J database and run a query to see if it is properly formatted. Returns FALSE if the database is not available or not properly formatted.
#' 
#' @param url A character string indicating the URL of the neo4j database.
#' @param username A character string indicating the username for the neo4j database (if needed).
#' @param password A character string indicating the password for the neo4j database (if needed).
#' @param warn A logical indicating whether to print a warning message if with failure information if the database is not available or not properly formatted. Default is TRUE.
#' 
#' @return TRUE if the database is available and properly formatted, FALSE otherwise.
#' @export
#' @examples
#' print(neo4j_engine_check("https://neo4j.monarchinitiative.org"))
#' print(neo4j_engine_check("https://no-such-db.monarchinitiative.org"))
#' 
neo4j_engine_check <- function(url,
						       username = NA,
						       password = NA,
							   warn = TRUE) {
	# this will throw an error if it cannot connect:
	# graph_conn <- neo2R::startGraph(url, username = username, password = password)

    # use try to see if we can successfully create a connection; return TRUE if successful, FALSE if not
	tryCatch({
		e <- neo4j_engine(url = url, username = username, password = password)
		# check to see if we can run a query, we'll just grab one random node; this should fail 
		# if something is wrong (e.g. the database is not in KGX format)
		cypher_query(e, "MATCH (n)-[r]->(q) RETURN n, r, q LIMIT 1")
		return(TRUE)
	}, error = function(e) {
		if(warn) {
			warning(e$message)
		}
		return(FALSE)
	}, warning = function(e) {
		if(warn) {
			warning(e$message)
		}
		return(FALSE)
	})
}
