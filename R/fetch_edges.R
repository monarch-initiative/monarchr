#' Expand a graph by fetching connected edges from the KG.
#'
#' @param g A graph
#' @param direction The direction of associations to fetch. Can be "in", "out", or "both". Default is "both".
#' @param predicates A vector of relationship predicates (nodes in g are subjects in the KG), indicating which edges to consider in the neighborhood. If NULL (default), all edges are considered.
#' @param result_categories A vector of node categories, indicating which nodes in the larger KG (objects in the relationship) to consider as potential result nodes. If NULL (default), all object nodes in the larger KG are considered as potential nodes.
#' @param transitive NOT IMPLEMENTED If TRUE, include transitive closure of the neighborhood. Default is FALSE. Useful in combination with predicates like `biolink:subclass_of`.
#' @param drop_unused_query_nodes If TRUE, remove query nodes from the result, unless they are at the neighboorhood boundary, ie, required for connecting to the result nodes. Default is FALSE.
#'
#' @return A tbl_kgx graph
#' @export
#' @examples
#' e <- neo4j_engine()
#' g <- e %>% field_search("id", "MONDO:0007525")
#' phenos <- g %>%
#'   fetch_edges(predicates = "biolink:has_phenotype", result_categories = "biolink:PhenotypicFeature")
#'
#' ancestors <- g %>%
#'  fetch_edges(predicates = "biolink:subclass_of", transitive = TRUE)
#'
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
fetch_edges <- function(obj, ...) {
	UseMethod("fetch_edges")
}
