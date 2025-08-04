#' Fetch nodes from a graph using a set of IDs or conditions
#'
#' This function fetches nodes (and no edges) from a knowledge graph engine based on a set of
#' conditions or a set of identifiers. If query_ids is provided, the function
#' will fetch nodes with the specified identifiers. If query_ids is NULL, the
#' function will fetch nodes based on the conditions provided. Only a limited
#' set of condition expressions are supported, see details.
#'
#' @details
#' If query_ids is provided, the function will fetch nodes with the specified.
#' If query_ids is NULL, the function will fetch nodes based on a condition
#' expression. The following features are supported:
#'
#' - Matching node properties with boolean operators, e.g. `in_taxon_label == "Homo sapiens"`.
#' - Matching multi-valued properties with `%in_list%`, e.g. `"biolink:Gene" %in_list% category`. NOTE: using `%in_list%` against vector queries, e.g. `in_taxon_label %in_list% c("Homo sapiens", "Mus musculus")` is *not* supported. Nor does `%in_list%` support multi-valued left hand sides; `c("biolink:Disease", "biolink:Gene") %in_list% category` will not work.
#' - Boolean connectives with `|`, `&`, and `!`, e.g. `in_taxon_lable == "Homo sapiens" | "biolink:Gene" %in_list% category`.
#'
#' If more than one condition parameter is specified, they are combined with `&`; for example,
#' `fetch_nodes(engine, in_taxon_lable == "Homo sapiens", "biolink:Gene" %in_list% category)` is equivalent to
#' `fetch_nodes(engine, in_taxon_lable == "Homo sapiens" & "biolink:Gene" %in_list% category)`.
#' @param engine A graph engine object
#' @param ... A set of conditions identifying the nodes to fetch, only used if query_ids is NULL
#' @param query_ids A character vector of identifiers to fetch
#' @param limit An integer specifying the maximum number of nodes to fetch. Default to NULL, no limit.
#' @return A tbl_kgx object containing the nodes
#'
#' @examples
#' library(tidygraph)
#' library(dplyr)
#'
#' @examplesIf monarch_engine_check()
#' monarch_engine() |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' # a large query
#' monarch_engine() |>
#'   fetch_nodes("biolink:Disease" %in_list% category)
#'
#' @examples
#' # file_engine supports the same features as neo4j_engine
#' # (using the example KGX file packaged with monarchr)
#' data(eds_marfan_kg)
#'
#' eds_marfan_kg |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' # grab all Homo sapiens genes
#' eds_marfan_kg |>
#'   fetch_nodes(in_taxon_label == "Homo sapiens" & "biolink:Gene" %in_list% category)
#'
#' @export
fetch_nodes <- function(engine, ..., query_ids = NULL, limit = NULL) {
    UseMethod("fetch_nodes")
}
