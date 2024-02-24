#' Create a KGX graph object
#'
#' This function creates a new tbl_kgx object which inherits from tidygraph::tbl_graph, from node and edge dataframes, ensuring they conform to the KGX specification 
#' described at https://github.com/biolink/kgx/blob/master/specification/kgx-format.md. Specifically, nodes must have an 'id' and 'category' column,
#' and edges, if provided, must have 'subject', 'predicate', and 'object' columns. The function allows graphs with no edges.
#' The function sets 'from' and 'to' columns in the edges from 'subject' and 'object' respectively, and sets the node key to 'id'.
#' Additional columns are allowed.
#' 
#' This function will generally be called internally.
#'
#' @param nodes A data frame containing the nodes of the graph. Must have 'id' and 'category' columns.
#' @param edges A data frame containing the edges of the graph. Must have 'subject', 'predicate', and 'object' columns. Can be NULL.
#' @param ... Additional arguments passed to the function.
#' @return A KGX graph object.
#' @examples
#' nodes <- data.frame(id = c("A", "B"), category = c("gene", "disease"))
#' edges <- data.frame(subject = c("A"), predicate = c("associated_with"), object = c("B"))
#' g <- tbl_kgx(nodes, edges)
#' @export
#' @importFrom tidygraph tbl_graph
tbl_kgx <- function(nodes = NULL, edges = NULL, ...) {
	if(is.null(nodes$id)) { stop("Error: tbl_kgx nodes must have an 'id' column.") }
	if(is.null(nodes$id)) { stop("Error: tbl_kgx nodes must have an 'category' column.") }

	# we do allow graphs with no edges
	if(!is.null(edges)) {
		if(is.null(edges$subject)) { stop("Error: tbl_kgx edges must have an 'subject' column.") }
		if(is.null(edges$predicate)) { stop("Error: tbl_kgx edges must have an 'predicate' column.") }
		if(is.null(edges$object)) { stop("Error: tbl_kgx edges must have an 'object' column.") }

		# set canonical to and from columns
		edges$from <- edges$subject
		edges$to <- edges$object
	}

	g <- tidygraph::tbl_graph(nodes = nodes, edges = edges, node_key = "id")
	class(g) <- c("tbl_kgx", class(g))
	return(g)
}