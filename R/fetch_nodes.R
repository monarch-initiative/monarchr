#' Fetch nodes from a graph
#' 
#' This function fetches nodes from a graph based on a set of conditions.
#' 
#' @param engine A graph engine object
#' @param ... A set of conditions
#' @return A tbl_kgx object containing the nodes
#' @examples
#' e <- neo4j_engine()
#' g <- fetch_nodes(e, id == "MONDO:0007525")
#' g <- fetch_nodes(e, "biolink:Disease" %in% category | "biolink:Gene" %in% category)
#' @export
fetch_nodes <- function(engine, ...) {
    UseMethod("fetch_nodes")
}