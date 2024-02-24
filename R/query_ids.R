
#' Query IDs
#'
#' Given a vector of id values, returns a tbl_kgx graph with corresponding nodes from the knowledge graph. No edges are returned.
#'
#' @param ids A vector of IDs to be queried.
#' @return A tbl_kgx graph.
#' @examples
#' ids <- c("MONDO:0015780", "HP:0001903", "HGNC:2890")
#' g <- query_ids(ids)
#' @export
query_ids <- function(ids) {
  # if ids is a length-1 character vector, it is treated as a scalar, so we need to wrap it in a list
  if (length(ids) == 1) {
    ids <- list(ids)
  }
  res <- cypher_query(query = "MATCH (n) WHERE n.id IN $ids RETURN n",
                      parameters = list(ids = ids))
  return(res)
}