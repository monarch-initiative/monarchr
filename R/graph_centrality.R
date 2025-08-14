#' Add centrality
#'
#' First computes of each node in a graph.
#' Then adds the centrality score as an node attribute.
#' @param fun The centrality function to use.
#'  Default is \link[igraph]{harmonic_centrality}.
#' @param col Name of the new node attribute to store the centrality score.
#' @param ... Additional arguments passed to the centrality function
#'  (\code{sim_fun}).
#' @inheritParams nodes
#' @returns Graph object with centrality added as a new node attribute.
#' @export
#' @importFrom tidygraph active
#' @importFrom tidygraph activate
#' @examples
#' data(eds_marfan_kg)
#' g <- eds_marfan_kg |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")|>
#'           expand(categories = "biolink:Gene")
#' g <- graph_centrality(g)
#' nodes(g)$centrality
graph_centrality <- function(graph,
														 fun=igraph::harmonic_centrality,
														 col="centrality",
														 ...){
	active_tbl <- active(graph)
	message("Computing node centrality.")
	graph <- graph|>
		activate(nodes)|>
		dplyr::mutate(!!col:=fun(graph, ...)) |>
		activate(!!rlang::sym(active_tbl))
	return(graph)
}
