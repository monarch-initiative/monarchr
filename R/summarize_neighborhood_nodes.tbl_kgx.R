#' @export
summarize_neighborhood_nodes.tbl_kgx <- function(graph, ...) {
	# check to see if g has a last_engine attribute
	if(!is.null(attr(graph, "last_engine"))) {
		engine <- attr(graph, "last_engine")
		return(summarize_neighborhood_nodes(engine, graph, ...))
	} else {
		stop("Error: tbl_kgx object does not have a most recent engine. Use summarize_neighborhood_nodes(engine, g, ...) instead.")
	}
}
