#' Join two KGX graphs by their nodes and edges.
#'
#' Given two KGX graphs, returns a new KGX graph that is the union of the two input graphs,
#' with any edges between nodes repeated for aother nodes with the same subject and object `id`.
#' The engine of the first graph is used for the new graph.
#'
#' This function first computes new node and edge data, by taking the full natural join of
#' node and edge data from the two input graphs, and then keeping unique rows. Note that nodes with
#' the same `id` that differ in any shared column are effectively kept as separate, taken
#' to represent the same entity in different contexts. (However, a node with an additional property will be
#' merged with a node without that property, as defined by the natural join.) In these
#' cases, any edge that connects to one of these nodes is also valid for the other node, and so the
#' method repeats edges across nodes with the same `id`.
#'
#'
#' @param graph1 A `tbl_kgx()` graph.
#' @param graph2 A `tbl_kgx()` graph.
#' @param ... Other parameters (not used)
#'
#' @return A `tbl_kgx()` graph
#' @export
#' @examples
#' ## Using local MONDO KGX file (packaged with monarchr)
#' monarch <- file_engine(system.file("extdata", "mondo_kgx_tsv.tar.gz",
#'                               package = "monarchr"))
#'
#' eds_and_phenos <- monarch |>
#'                   fetch_nodes(query_ids = "MONDO:0007525") |>
#'                   expand(predicates = "biolink:has_phenotype",
#'                          categories = "biolink:PhenotypicFeature")
#'
#' marfan_and_phenos <- monarch |>
#'                      fetch_nodes(query_ids = "MONDO:0007947") |>
#'                      expand(predicates = "biolink:has_phenotype",
#'                             categories = "biolink:PhenotypicFeature")
#'
#' combined <- graph_join(eds_and_phenos, marfan_and_phenos)
#' print(combined)
kg_join <- function(graph1, graph2, ...) {
  UseMethod("kg_join")
}
