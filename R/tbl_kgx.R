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
#' @param attach_engine An engine to attach to the newly created graph for use in future queries based on the graph.
#' @param ... Additional arguments passed to the function.
#' @return A KGX graph object.
#' @examples
#' nodes <- data.frame(id = c("A", "B"), category = c("gene", "disease"))
#' edges <- data.frame(subject = c("A"), predicate = c("associated_with"), object = c("B"))
#' g <- tbl_kgx(nodes, edges)
#' @export
#' @importFrom tidygraph tbl_graph
tbl_kgx <- function(nodes = NULL, edges = NULL, attach_engine = NULL, ...) {
	# nodes can be empty, if so we need to create an empty data frame
	if(nrow(nodes) == 0) {
		nodes <- data.frame(id = character(0))
		nodes$category <- list()
		edges <- data.frame(subject = character(0), predicate = character(0), object = character(0))
	 }

	if(is.null(nodes$id)) { stop("Error: tbl_kgx nodes must have an 'id' column.") }
	if(is.null(nodes$id)) { stop("Error: tbl_kgx nodes must have an 'category' column.") }

	# we do allow graphs with no edges
	if(!is.null(edges)) {
		if(is.null(edges$subject)) { stop("Error: tbl_kgx edges must have an 'subject' column.") }
		if(is.null(edges$predicate)) { stop("Error: tbl_kgx edges must have an 'predicate' column.") }
		if(is.null(edges$object)) { stop("Error: tbl_kgx edges must have an 'object' column.") }

		# set canonical to and from columns from subject and object if they don't already exist
		if(!"from" %in% colnames(edges)) {
			edges$from <- edges$subject
		}
		if(!"to" %in% colnames(edges)) {
			edges$to <- edges$object
		}
	} else {
		# but if given no edges, we spec out subject, predicate, object cols at least (and to and from)
		edges <- data.frame(subject = character(), predicate = character(), object = character(),
												to = character(), from = character())
	}

	g <- tidygraph::tbl_graph(nodes = nodes, edges = edges, node_key = "id")

	attr(g, "last_engine") <- attach_engine
	class(g) <- c("tbl_kgx", class(g))

	# if we have an engine, use its preferences for setting df column order
	# (order_cols uses the last attached engine)
	if(!is.null(attach_engine)) {
		g <- order_cols(g)
	}

	return(g)
}
