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
#' @examplesIf monarch_engine_check()
#' ## Using Monarch (hosted)
#' phenos <- monarch_engine() |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  result_categories = "biolink:PhenotypicFeature")
#'
#' print(phenos)
#'
#'
#'
#' @examples
#' ## Using local MONDO KGX file (packaged with monarchr)
#' phenos <- file_engine(system.file("extdata", "mondo_kgx_tsv.tar.gz",
#'                       package = "monarchr")) |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  result_categories = "biolink:PhenotypicFeature")
#'
#' print(phenos)
#'
#'
#'
#' @examplesIf file_engine_check("https://kghub.io/kg-obo/mondo/2024-03-04/mondo_kgx_tsv.tar.gz")
#' ## Using MONDO KGX file (remote) as an example
#' phenos <- file_engine("https://kghub.io/kg-obo/mondo/2024-03-04/mondo_kgx_tsv.tar.gz") |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  result_categories = "biolink:PhenotypicFeature")
#'
#' print(phenos)
#'
#' file.remove("mondo_kgx_tsv.tar.gz") # cleanup - remove the downloaded file
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
expand <- function(engine = NULL,
												graph,
												direction = "both",
												predicates = NULL,
												result_categories = NULL,
												transitive = FALSE,
												drop_unused_query_nodes = FALSE) {
	UseMethod("expand")
}
