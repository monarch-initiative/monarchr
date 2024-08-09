
#' Load a graph from a KGX-formatted .tar.gz file.
#'
#' Given a KGX-formatted tabular KG
#' (see https://github.com/biolink/kgx/blob/master/specification/kgx-format.md)
#' loads it as a graph.
#'
#' @param filename File to the graph from. Must end in .tar.gz and conform to KGX specification (see description).
#' @param attach_engine An engine to attach to the graph (optional).
#' @param ... Other parameters (unused)
#' @return A `tbl_kgx` graph.
#' @export
#' @examplesIf monarch_engine_check()
#' phenos <- monarch_engine() |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")
#'
#' save_kgx(phenos, "phenos.tar.gz")
#'
#' # when loading the graph, we can optionally attach an engine
#' loaded_phenos <- load_kgx("phenos.tar.gz", attach_engine = monarch_engine())
#'
#' loaded_phenos
#'
#' # cleanup saved file
#' file.remove("phenos.tar.gz")
load_kgx <- function(filename, attach_engine = NULL, ...) {
	e <- file_engine(filename)
	g <- e$graph

	attr(g, "last_engine") <- attach_engine
	g <- order_cols(g)
	return(g)
}
