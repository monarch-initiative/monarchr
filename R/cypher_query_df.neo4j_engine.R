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
cypher_query_df.neo4j_engine <- function(engine, query, parameters = list(), ...) {
    # kg_name <- "monarch" # we can parameterize this in the future if desired

	# pkg_env <- parent.env(environment())
	# graph_connections <- get("graph_connections", envir = pkg_env)
    result <- neo2R::cypher(engine$graph_conn, query = query, parameters = parameters, result = "row", arraysAsStrings = FALSE)

    return(result)
}
