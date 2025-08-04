#' @export
#' @importFrom tidygraph as_tibble
nodes.tbl_kgx <- function(graph, ...) {
  tidygraph::as_tibble(graph, active = "nodes")
}


#' @export
#' @importFrom tidygraph as_tibble
edges.tbl_kgx <- function(graph, ...) {
  tidygraph::as_tibble(graph, active = "edges")
}


#' Get graph nodes table.
#'
#' @param graph A graph object
#' @param ... Other options (unused)
#' @return A tibble with the nodes of the graph
#' @importFrom tidygraph as_tibble
#' @export
#' @examples
#' # (using the example KGX file packaged with monarchr)
#' data(eds_marfan_kg)
#'
#' g <- eds_marfan_kg |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' print(nodes(g))
nodes <- function(graph, ...) {
  UseMethod("nodes")
}

#' Get graph edges table.
#'
#' @param graph Input graph
#' @param ... Other options (unused)
#' @export
#' @return A tibble with the edges of the graph
#' @importFrom tidygraph as_tibble
#' @examples
#' # (using the example KGX file packaged with monarchr)
#' data(eds_marfan_kg)
#'
#' g <- eds_marfan_kg |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' print(edges(g))
edges <- function(graph, ...) {
  UseMethod("edges")
}

#' Explode a graph into a list of single-node graphs
#'
#' @param graph A tbl_kgx graph.
#' @param ... Other options (unused)
#' @return A list of tbl_kgx graphs.
#' @examples
#' # (using the example KGX file packaged with monarchr)
#' data(eds_marfan_kg)
#'
#' g <- eds_marfan_kg |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' print(explode(g))
#' @export
explode <- function(graph, ...) {
  UseMethod("explode")
}

#' @export
#' @importFrom tidygraph activate
explode.tbl_kgx <- function(graph, ...) {
  nodes <- as_tibble(graph, active = "nodes")
  graphs <- lapply(nodes$id, function(node_id) {
    filter(tidygraph::activate(graph, nodes), id == node_id)
  })
  return(graphs)
}



#' @noRd
merge_lists <- function(a, b) {
	# This function takes two named lists and merges them recursively. Values in
	# the second list (`b`) override or extend those in the first list (`a`).
	# If both `a` and `b` contain a named list at the same position, they are merged recursively.
	#
	# a <- list(x = list(y = 1, z = 2), foo = "bar")
	# b <- list(x = list(y = 42, new = 99), foo = "baz", extra = "new_value")
	# merge_lists(a, b)

	for (name in names(b)) {
		if (is.list(b[[name]]) && name %in% names(a) && is.list(a[[name]])) {
			# Recursively merge if both elements are lists
			a[[name]] <- merge_lists(a[[name]], b[[name]])
		} else {
			# Otherwise, override/add the value from b to a
			a[[name]] <- b[[name]]
		}
	}
	return(a)
}
