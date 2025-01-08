internal_cypher_query_df <- function(engine, query, parameters = NULL, ...) {
	if(length(query) == 1) {
		result <- neo2R::cypher(engine$graph_conn, query = query, parameters = parameters, result = "row", arraysAsStrings = FALSE)
	} else {
		result <- neo2R::multicypher(engine$graph_conn, queries = query, parameters = parameters, result = "row", arraysAsStrings = FALSE)
	}

	return(result)
}

#internal_cypher_query_df_memoised <- memoise::memoise(internal_cypher_query_df)

#' @export
#' @importFrom neo2R cypher
#' @importFrom neo2R multicypher
#' @importFrom memoise memoise
cypher_query_df.neo4j_engine <- function(engine, query, parameters = NULL, ...) {
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

		internal <- memoise::memoise(internal_cypher_query_df, cache = engine$cache)
		res <- internal(engine_copy, query, parameters, ...)

		return(res)
	} else {
		internal_cypher_query_df(engine, query, parameters, ...)
	}
}
