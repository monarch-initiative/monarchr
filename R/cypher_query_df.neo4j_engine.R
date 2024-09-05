#' @export
#' @importFrom neo2R cypher
#' @importFrom neo2R multicypher
cypher_query_df.neo4j_engine <- function(engine, query, parameters = NULL, ...) {
	if(length(query) == 1) {
    result <- neo2R::cypher(engine$graph_conn, query = query, parameters = parameters, result = "row", arraysAsStrings = FALSE)
	} else {
		result <- neo2R::multicypher(engine$graph_conn, queries = query, parameters = parameters, result = "row", arraysAsStrings = FALSE)
	}

  return(result)
}
