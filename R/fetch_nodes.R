#' Fetch nodes from a graph
#' 
#' This function fetches nodes from a graph based on a set of conditions.
#' 
#' @param engine A graph engine object
#' @param ... A set of conditions identifying the nodes to fetch, only used if query_ids is NULL
#' @param query_ids A character vector of identifiers to fetch
#' @return A tbl_kgx object containing the nodes
#' @examples
#' e <- neo4j_engine()
#' g <- fetch_nodes(e, id == "MONDO:0007525")
#' g <- fetch_nodes(e, "biolink:Disease" %in% category | "biolink:Gene" %in% category)
#' @export
fetch_nodes <- function(engine, ..., query_ids = NULL) {
    UseMethod("fetch_nodes")
}