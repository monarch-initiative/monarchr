#' Send a graph to Cytoscape
#'
#' Given a tbl_kgx graph, send it to Cytoscape for visualization. Node labels
#' are mapped to node `name` (if available, otherwise they default to node `id`),
#' node color is mapped to `pcategory`, edge color is mapped to `predicate`,
#' node hover-over text is set to `description` (if available, otherwise node `id`),
#' and edge hover-over text is set to `predicate`. Nodes are layed out
#' using the Kamada-Kawai method. These properties and more may be customized in
#' the Cytoscape application. This function requires that Cytoscape is installed
#' and running independently of the R session.
#'
#'
#' @param g A `tbl_kgx()` graph to visualize.
#' @param ... Other parameters (unused).
#'
#' @return NULL, invisibly
#' @export
#' @examplesIf FALSE
#' data(eds_marfan_kg)
#' g <- eds_marfan_kg |>
#' 	 fetch_nodes(query_ids = "MONDO:0020066") |>
#' 	 expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE) |>
#' 	 expand(categories = c("biolink:PhenotypicFeature", "biolink:Gene"))
#'
#' # Cytoscape must be installed and running
#' cytoscape(g)
#'
#' @import RCy3
#' @import tidygraph
#' @import dplyr
cytoscape <- function(g, ...) {
	UseMethod("cytoscape")
}
