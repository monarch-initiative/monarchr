#' @export
#' @importFrom magrittr %>%
#' @title Convert a tidygraph to a monarch_kg graph.
#'
#' @description Converts a tidygraph of the appropriate format into a monarch_kg graph. Not intended for direct use.
#'
#' @details The monarch_kg class extends tidygraph and defines Monarch-specific methods in addition to functionality provided by tidygraph and igraph.
#' @param graph_obj input to convert to a monarch_kg.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new monarch_kg.
#' @seealso \code{\link{monarch_search}}.
#' @examples
#' g <- monarch_search("Cystic Fibrosis", limit = 3)
#'
#' str(g)
#'
#' print(methods(class = "monarch_kg"))
as.monarch_kg <- function(graph_obj, ...) {
	if(!"tbl_graph" %in% class(graph_obj)) {
		stop("Cannot convert to minikg, input must be a tidygraph object.")
	}
	# TODO: should we validate that it's got the right columns?
	class(graph_obj) <- c("monarch_kg", class(graph_obj))
	return(graph_obj)
}
