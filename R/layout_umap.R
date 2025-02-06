#' Layout UMAP
#'
#' Generate a 2D or 2D layout of a graph using the UMAP algorithm.
#' See here for details:
#' \href{https://igraph.org/c/doc/igraph-Layout.html#igraph_layout_umap}{
#' igraph_layout_umap}
#' @param use_3d Logical, whether to use a 3D layout (TRUE) or
#'  2D layout (FALSE). Default is FALSE (2D).
#' @param prefix A character string prefix to add to the layout column names.
#' @param ... Additional arguments passed to the layout function.
#' @inheritParams nodes
#' @returns A matrix of x and y coordinates for each node in the graph.
#' @export
#' @examples
#' set.seed(2024)
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#' g <- file_engine(filename) |>
#'           fetch_nodes(pcategory=="biolink:Disease", limit=40) |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")|>
#'           tidygraph::sample_n(200) |>
#'           expand(categories = "biolink:Gene")
#' X <- layout_umap(g)
#' g <- graph_centrality(g)
#' plot(g, layout=X, node_size=centrality)
layout_umap <- function(graph,
												use_3d = FALSE,
												prefix="UMAP",
												...){
	if(use_3d){
		fun <- utils::getFromNamespace("layout_umap_3d_impl", "igraph")
	} else {
		fun <- utils::getFromNamespace("layout_umap_impl", "igraph")
	}
	X <- fun(graph, res=matrix())
	rownames(X) <- nodes(graph)$id
	colnames(X) <- paste0(prefix, 1:ncol(X))
	return(X)
}
