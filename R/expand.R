#' Fetch additional knowledge graph edges connected to a query graph
#'
#' Given an optional KG engine (e.g. a `file_engine()`,
#' `neo4j_engine()`, or `monarch_engine()`) and a query `tbl_kgx()` graph, fetches additional nodes and edges
#' from the KG, expanding the query graph according to specific criteria. If the first parameter is an engine, that
#' engine is used; if the first parameter is a query graph, the most recent engine associated with the graph is used.
#'
#'
#' @param graph A query `tbl_kgx()` graph ot query with.
#' @param engine (Optional) An engine to use for fetching query graph edges. If not provided, the graph's most recent engine is used.
#' @param direction The direction of associations to fetch. Can be "in", "out", or "both". Default is "both".
#' @param predicates A vector of relationship predicates (nodes in g are subjects in the KG), indicating which edges to consider in the neighborhood. If NULL (default), all edges are considered.
#' @param categories A vector of node categories, indicating which nodes in the larger KG may be fetched. If NULL (default), all nodes in the larger KG are will be fetched.
#' @param transitive If TRUE, include transitive closure of the neighborhood. Default is FALSE. Useful in combination with predicates like `biolink:subclass_of`.
#' @param ... Other parameters passed to methods.
#'
#' @return A `tbl_kgx()` graph
#' @export
#' @examplesIf monarch_engine_check()
#' ## Using Monarch (hosted)
#' phenos <- monarch_engine() |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")
#'
#' print(phenos)
#'
#'
#'
#' @examples
#' ## Using example KGX file packaged with monarchr
#' data(eds_marfan_kg)
#' phenos <- eds_marfan_kg |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")
#'
#' print(phenos)
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
expand <- function(graph,
  								 engine = NULL,
									 direction = "both",
									 predicates = NULL,
									 categories = NULL,
									 transitive = FALSE,

									 					 ...) {
	UseMethod("expand")
}
