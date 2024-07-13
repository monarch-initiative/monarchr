#' Specialized plot() function for KGX graphs
#'
#' @export
#' @param g A tbl_kgx graph.
#' @param ... other parameters (not used).
#' @import ggraph
#' @import ggplot2
#' @importFrom stringr str_wrap
plot.tbl_kgx <- function(g, ...) {
	g <- ggraph(g, layout = "sugiyama") +
		geom_edge_link(aes(color = predicate),
									 arrow = arrow(length = unit(2, 'mm'), type = "closed"),
									 end_cap = circle(2.5, 'mm')) +
		geom_node_point(aes(color = pcategory),
										size = 3) +
		geom_node_label(aes(label = str_wrap(name, 20)),
										size = 2,
										repel = TRUE,
										check_overlap = TRUE,
										fill = "#FFFFFF88")

	return(g)
}
