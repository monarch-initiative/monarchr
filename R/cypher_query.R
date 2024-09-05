#' Execute a Cypher Query
#'
#' This function takes a Cypher query and parameters, executes the query using the given engine, and returns the result as a tbl_kgx graph.
#'
#' @param engine A neo4j KG engine
#' @param query A string representing the Cypher query. Multiple queries may be passed as a vector; if so, Neo2R::multicypher if used and the result is returned as a single joined graph.
#' @param parameters A list of parameters for the Cypher query. Default is an empty list.
#' @param ... Additional arguments passed to the function.
#' @return The result of the Cypher query as a tbl_kgx graph.
#' @export
#' @examplesIf monarch_engine_check()
#' engine <- monarch_engine()
#'
#' query <- "MATCH (n) WHERE n.id IN $ids RETURN n LIMIT 10"
#' ids <- c("MONDO:0007525", "MONDO:0020066", "MONDO:0034021")
#' parameters <- list(ids = ids)
#'
#' result <- cypher_query(engine, query, parameters)
#' print(result)
#' @importFrom neo2R cypher
cypher_query <- function(engine, query, parameters = NULL, ...) {
    UseMethod("cypher_query")
}

