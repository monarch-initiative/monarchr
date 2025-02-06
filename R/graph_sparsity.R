#' Compute sparsity
#'
#' Compute sparsity (proportion of zero values) in a matrix or graph.
#' @param x An igraph object or a (sparse) matrix.
#' @param fun Function to convert graph to matrix.
#' @param ... Arguments passed to \code{fun}.
#' @returns A numeric value representing the proportion of zero
#'	values in the graph/matrix.
#' @export
#' @examples
#' ## Using example KGX file packaged with monarchr
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#' g <- file_engine(filename) |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")
#'
#' graph_sparsity(g)
graph_sparsity <- function(x,
										 fun=igraph::as_adjacency_matrix,
										 ...){
	if(is(x,"igraph")){
		x <- fun(x)
	}
	if(is(x,"Matrix")||is(x,"sparseMatrix")){
		return(sum(x==0)/length(x))
	}
	else{
		stop("x must be an igraph object or a (sparse) matrix.")
	}
}
