#' Execute a Cypher Query
#'
#' This function takes a Cypher query and parameters, executes the query using the given engine, and returns the result as a data frame.
#'
#' @param engine A neo4j_engine() or derivative providing access to a Neo4j database.
#' @param query A string representing the Cypher query, which should return a table. Multiple queries may be passed as a vector; if so, Neo2R::multicypher if used and the result is returned as a list of data frames.
#' @param parameters A list of parameters for the Cypher query, if required.
#' @param ... Additional arguments passed to the function.
#' @return The result of the Cypher query as a data frame, or a list of data frames if multiple queries are passed.
#' @export
#' @examplesIf monarch_engine_check()
#' engine <- monarch_engine()
#'
#' query <- "MATCH (n) WHERE n.id IN $ids RETURN n LIMIT 10"
#' parameters <- list(ids = c("MONDO:0007525", "MONDO:0020066", "MONDO:0034021"))
#'
#' result <- cypher_query_df(engine, query, parameters)
#' print(result)
#' @importFrom neo2R cypher
cypher_query_df <- function(engine, query, parameters = NULL, ...) {
    UseMethod("cypher_query_df")
}
