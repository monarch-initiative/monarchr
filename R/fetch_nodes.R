#' Fetch nodes from a graph
#' 
#' This function fetches nodes from a graph based on a set of conditions.
#' 
#' @param engine A graph engine object
#' @param id A character vector of identifiers to fetch
#' @param ... A set of conditions identifying the nodes to fetch, only used if id is NULL
#' @return A tbl_kgx object containing the nodes
#' @examples
#' e <- neo4j_engine()
#' g <- fetch_nodes(e, id == "MONDO:0007525")
#' g <- fetch_nodes(e, "biolink:Disease" %in% category | "biolink:Gene" %in% category)
#' @export
fetch_nodes <- function(engine, id = NULL, ...) {
    UseMethod("fetch_nodes")
}