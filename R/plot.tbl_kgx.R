#' Specialized plot() function for KGX graphs
#'
#' @export
#' @param g A tbl_kgx graph.
#' @param layout The layout to use for the plot. Default is "sugiyama".
#' @param node_color The column to use for node color. Default is pcategory.
#' @param edge_color The column to use for edge color. Default is predicate.
#' @param ... other parameters (not used).
#' @import ggraph
#' @import ggplot2
#' @importFrom stringr str_wrap
plot.tbl_kgx <- function(g, layout = "auto", node_color = pcategory, edge_color = predicate, ...) {
	g <- ggraph(g, layout = layout) +
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

	return(g)
}
