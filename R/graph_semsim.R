#' Add semantic similarity
#'
#' First computes semantic similarity between all pairs of nodes in a graph.
#' Then adds the continuous similarity score as an edge attribute.
#' @param fun The similarity function to use.
#'  Default is \link[igraph]{similarity}.
#' @param col Name of the new edge attribute to store the similarity score.
#' @param nm The node attribute to use as the matrix row/colnames.
#' @param return_matrix Return the similarity matrix instead of the graph.
#' @param sparse Return a sparse matrix instead of a dense matrix.
#' @param ... Additional arguments passed to the similarity function
#'  (\code{fun}).
#' @inheritParams nodes
#' @returns Graph object with similarity added as a new edge attribute.
#' @export
#' @examples
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#' g <- file_engine(filename) |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")|>
#'           expand(categories = "biolink:Gene")
#' g <- graph_semsim(g)
#' edges(g)$similarity
graph_semsim <- function(graph,
												 fun=igraph::similarity,
												 col="similarity",
												 nm="id",
												 sparse=TRUE,
												 return_matrix=FALSE,
												 ...){
	from <- to <- NULL;
	message("Computing pairwise node similarity.")
	X <- fun(graph, ...)
	if(sparse) {
		requireNamespace("Matrix")
		X <- Matrix::Matrix(X, sparse=TRUE)
	}
	rownames(X) <- colnames(X) <- nodes(graph)[[nm]]
	if(return_matrix) return(X)

	graph <- graph|>
		activate(edges)|>
		dplyr::mutate(!!col:=purrr::map2_dbl(from, to, ~ X[.y, .x]))
	return(graph)
}
