#' @export
#' @importFrom neo2R cypher
cypher_query_df.neo4j_engine <- function(engine, query, parameters = list(), ...) {
    result <- neo2R::cypher(engine$graph_conn, query = query, parameters = parameters, result = "row", arraysAsStrings = FALSE)

    return(result)
}
