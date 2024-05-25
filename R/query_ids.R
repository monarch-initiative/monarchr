# generic function for query_ids
#' Query IDs
#' 
#' Given a vector of id values, returns a tbl_kgx graph with corresponding 
#' nodes from the knowledge graph. No edges are returned.
#' 
#' @param engine A graph engine object.
#' @param ids A vector of IDs to be queried.
#' @return A tbl_kgx graph.
#' @examples
#' ids <- c("MONDO:0015780", "HP:0001903", "HGNC:2890")
#' g <- query_ids(ids)
#' @export
query_ids <- function(engine, ids) {
    UseMethod("query_ids")
}