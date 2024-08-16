#' Set edge/row data column order according to most recent engine preferences
#' @param g A `tbl_kgx` graph.
#' @import dplyr
#' @import tidygraph
order_cols <- function(g) {
	e <- attr(g, "last_engine")
	node_prefs <- c("id", "pcategory", "name")
	edge_prefs <- c("subject", "predicate", "object")

	if(!is.null(e)) {
		node_prefs <- e$preferences$node_property_priority
		edge_prefs <- e$preferences$edge_property_priority
	}

	current_node_names <- colnames(nodes(g))
	used_prefs_node_names <- node_prefs[node_prefs %in% current_node_names]
	set_node_names <- c(used_prefs_node_names, setdiff(current_node_names, used_prefs_node_names))

	current_edge_names <- colnames(edges(g))
	used_prefs_edge_names <- edge_prefs[edge_prefs %in% current_edge_names]
	set_edge_names <- c(used_prefs_edge_names, setdiff(current_edge_names, used_prefs_edge_names))

	res <- g |>
		activate(nodes) |>
		select(all_of(set_node_names)) |>
		activate(edges) |>
		select(all_of(set_edge_names)) |>
		activate(nodes)

	return(res)
}
