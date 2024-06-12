#' Fetch additional knowledge graph edges connected to a query graph
#'
#' Given an optional KG engine (e.g. a `file_engine()`,
#' `neo4j_engine()`, or `monarch_engine()`) and a query `tbl_kgx()` graph, fetches additional nodes and edges
#' from the KG, expanding the query graph according to specific criteria. If the first parameter is an engine, that
#' engine is used; if the first parameter is a query graph, the most recent engine associated with the graph is used.
#'
#'
#' @param engine (Optional) An engine to use for fetching query graph edges.
#' @param graph A query `tbl_kgx()` graph to query from.
#' @param direction The direction of associations to fetch. Can be "in", "out", or "both". Default is "both".
#' @param predicates A vector of relationship predicates (nodes in g are subjects in the KG), indicating which edges to consider in the neighborhood. If NULL (default), all edges are considered.
#' @param result_categories A vector of node categories, indicating which nodes in the larger KG may be fetched. If NULL (default), all nodes in the larger KG are will be fetched.
#' @param transitive If TRUE, include transitive closure of the neighborhood. Default is FALSE. Useful in combination with predicates like `biolink:subclass_of`.
#' @param drop_unused_query_nodes If TRUE, remove query nodes from the result, unless they are at the neighborhood boundary, i.e., required for connecting to the result nodes. Default is FALSE.
#'
#' @return A `tbl_kgx()` graph
#' @export
#' @examples
#' e <- monarch_engine()
#' g <- e %>% fetch_nodes(query_ids = "MONDO:0007525")
#' phenos <- g %>%
#'   fetch_edges(predicates = "biolink:has_phenotype", result_categories = "biolink:PhenotypicFeature")
#'
#' ancestors <- g %>%
#'  fetch_edges(predicates = "biolink:subclass_of", direction = "out", transitive = TRUE)
#'
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
fetch_edges <- function(engine = NULL,
												graph,
												direction = "both",
												predicates = NULL,
												result_categories = NULL,
												transitive = FALSE,
												drop_unused_query_nodes = FALSE) {
	UseMethod("fetch_edges")
}
