#' Specialized \code{plot()} function for KGX graphs
#'
#' @export
#' @param g A \link{tbl_kgx} graph.
#' @param layout The layout to use for the plot. Default is "auto" as used by `ggraph`.
#' @param node_color The column to use for node color. Default is "pcategory".
#' @param node_shape The column to use for node shape Default is "namespace".
#' @param edge_color The column to use for edge color. Default is "predicate".
#' @param edge_linetype The column to use for edge line type. Default is "primary_knowledge_source".
#' @param node_label The column to use for node labels. Defaults to "name".
#' @param plot_ids Whether to show node IDs in node labels. Defaults to FALSE.
#' @param label_size Size of node label text. Default is 2.
#' @param fan_strength Fan strength in ggraph's geom_edge_fan, Default is 2.
#' @param node_alpha Alpha value for nodes, default 0.9.
#' @param edge_alpha Alpha value for edges, default 0.9.
#' @inheritDotParams ggraph::ggraph
#' @importFrom stringr str_wrap
#' @import ggplot2
#' @import dplyr
#' @import tidygraph
#' @import ggraph
#' @examples
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#' g <- file_engine(filename) |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")|>
#'           expand(categories = "biolink:Gene")
#' plot(g)
#' @export
plot.tbl_kgx <- function(g,
									...,
									layout = "auto",
									node_color = pcategory,
									node_shape = namespace,
									edge_color = predicate,
									edge_linetype = primary_knowledge_source,
									node_label = name,
									plot_ids = FALSE,
							    label_size = 2,
									fan_strength = 2,
									edge_alpha = 0.9,
									node_alpha = 0.9)
{
	edge_linetype_colname <- rlang::quo_name(rlang::enquo(edge_linetype))
	if(is.null(edges(g)[[edge_linetype_colname]])) {
	 	edge_linetype <- NULL
	 	warning(paste0("Edge attribute ", edge_linetype_colname, " not found for use in setting line type. Ignoring."))
	}

	node_shape_colname <- rlang::quo_name(rlang::enquo(node_shape))
	if(is.null(nodes(g)[[node_shape_colname]])) {
		node_shape = NULL
		warning(paste0("Node attribute ", node_shape_colname, " not found for use in setting node shape. Ignoring."))
	} else {
		# although we don't expect any NA values, we don't want any that are NA to have no points (the default),
		# so map them to character "NA" for plotting
		g <- g |>
			activate(nodes) |>
			mutate({{node_shape}} := ifelse(is.na({{node_shape}}), "NA", {{node_shape}}))
	}


	node_label_colname <- rlang::quo_name(rlang::enquo(node_label))
	if(node_label_colname == "name" && !"name" %in% colnames(nodes(g))) {
		node_label = sym("id")
	}

	g <- g |>
		activate(nodes) |>
		mutate(plot_name := stringr::str_wrap({{node_label}}, 20))

	if(plot_ids == TRUE) {
		g <- g |>
			activate(nodes) |>
			mutate(plot_name = stringr::str_wrap(paste0(plot_name, " (", id, ")"), 20))
	}


	p <- ggraph(g, layout = layout, ...) +
		geom_edge_fan(mapping = aes(color = {{edge_color}},
																edge_linetype = {{edge_linetype}}),
									arrow = arrow(length = unit(2, 'mm'),
																type = "open"),
									end_cap = circle(2.5, 'mm'),
									alpha = edge_alpha,
									strength = fan_strength,
									width = 0.5) +
	  geom_node_point(mapping = aes(color = {{node_color}},
	  															shape = {{node_shape}}),
	  								alpha = node_alpha,
	  								size = 3) +
		geom_node_label(mapping = aes(label = plot_name),
										box.padding = 0.4,
										min.segment.length=0,
										size = label_size,
										repel = TRUE,
										segment.colour = alpha("black", 0.8),
										segment.linetype = "dotted",
										fill = "#FFFFFF88")

  return(p)
}
