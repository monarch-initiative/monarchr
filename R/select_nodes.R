#' Select nodes from a graph
#'
#' This is the generic function for `select_nodes`, which selects nodes from a graph
#' based on specific conditions. The conditions are specified through the `...` argument.
#' The actual implementation of the function depends on the class of the `graph` object,
#' and should be defined in specific methods for the `select_nodes` function.
#'
#' @param graph A graph object from which nodes should be selected.
#'   The specific class of this object determines the method that will be used.
#' @param ... Further arguments defining the conditions for selecting nodes.
#'   These are passed to the specific `select_nodes` method for the class of `graph`.
#'
#' @return A graph with a subset of nodes selected.
#'
#' @export
select_nodes <- function(graph, ...) {
	UseMethod("select_nodes")
}

#' Select nodes from a `monarch_kg` graph
#'
#' This is the method for `select_nodes` specific to `monarch_kg` graph objects.
#' It selects nodes based on the conditions specified in the `...` argument,
#' which are passed to `dplyr::filter`.
#'
#' @param graph An object of class `monarch_kg` from which nodes should be selected.
#' @param ... Further arguments passed to `dplyr::filter`. These define the conditions
#'   for selecting nodes based on their properties.
#'
#' @return A `monarch_kg` graph object with the nodes selected.
#'
#' @examples
#' g <- monarch_search("Diabetes")
#' g <- select_nodes(g, !is.na(inheritance_name) | id == "MONDO:0004782")
#'
#' @export
select_nodes.monarch_kg <- function(graph, ...) {
	expr <- rlang::enexprs(...)

	res <- mutate(activate(graph, nodes), selected = !! expr[[1]])
	return(res)
}
