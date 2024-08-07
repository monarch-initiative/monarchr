#' Specialized plot() function for KGX graphs
#'
#' @export
#' @param x A tbl_kgx graph.
#' @param ... other parameters (not used).
#' @param layout The layout to use for the plot. Default is "sugiyama".
#' @param node_color The column to use for node color. Default is pcategory.
#' @param edge_color The column to use for edge color. Default is predicate.
#' @import ggraph
#' @import ggplot2
#' @importFrom stringr str_wrap
plot.tbl_kgx <- function(x, ..., layout = "auto", node_color = pcategory, edge_color = predicate) {
	x <- ggraph(x, layout = layout) +
		geom_edge_fan(aes(color = {{edge_color}}),
									 arrow = arrow(length = unit(2, 'mm'), type = "closed"),
									 end_cap = circle(2.5, 'mm')) +
		geom_edge_loop(aes(color = {{edge_color}}),
									 arrow = arrow(length = unit(2, 'mm'), type = "closed"),
									 end_cap = circle(2.5, 'mm')) +
		geom_node_point(aes(color = {{node_color}}),
										size = 3) +
		geom_node_label(aes(label = str_wrap(name, 20)),
										size = 2,
										repel = TRUE,
										fill = "#FFFFFF88")

	return(x)
}
