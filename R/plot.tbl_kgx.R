#' Specialized \code{plot()} function for KGX graphs
#'
#' @export
#' @param x A \link{tbl_kgx} graph.
#' @param layout The layout to use for the plot. Default is "sugiyama".
#' @param node_color The column to use for node color. Default is "pcategory".
#' @param edge_color The column to use for edge color. Default is "predicate".
#' @param node_shape The column to use for node shape. Default is "pcategory".
#' @param node_size The column to use for node size. Default is NULL.
#' @param node_alpha The alpha value for nodes. Default is 0.9.
#' @param edge_linetype The column to use for edge linetype. Default is NULL.
#' @param edge_alpha The alpha value for edges. Default is 0.5.
#' @param bundle Whether to bundle edges with
#' \link[ggraph]{geom_edge_bundle_force2}. Default is FALSE.
#' @inheritParams theme_monarch
#' @inheritParams ggrepel::geom_label_repel
#' @inheritDotParams ggraph::ggraph
#' @import ggraph
#' @import ggplot2
#' @importFrom stringr str_wrap
#' @examples
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#' g <- file_engine(filename) |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")|>
#'           expand(categories = "biolink:Gene")
#' plot(g)
plot.tbl_kgx <- function(x,
												 ...,
												 layout = "auto",
												 node_color = pcategory,
												 node_shape = pcategory,
												 node_size = NULL,
												 node_alpha = .9,
												 edge_color = predicate,
												 edge_linetype = NULL,
												 edge_alpha = .5,
												 max.overlaps = 10,
												 bundle = FALSE,
												 palettes = monarch_palettes(),
												 layer_args =
												 	list(
												 		theme_bw=list(),
												 		theme=list(
												 			axis.text = ggplot2::element_blank(),
												 			axis.ticks = ggplot2::element_blank(),
												 			axis.title = ggplot2::element_blank(),
												 			legend.key = ggplot2::element_blank(),
												 			panel.background = ggplot2::element_blank(),
												 			panel.border = ggplot2::element_blank(),
												 			panel.grid = ggplot2::element_blank()
												 		),
												 		scale_edge_color_manual=list(
												 			values = palettes$edges$discrete,
												 			na.value = "grey"),
												 		scale_edge_color_gradientn=list(
												 			colors=palettes$continuous[3:1],
												 			na.value = "grey"),
												 		scale_color_manual=list(
												 			values = palettes$nodes$discrete,
												 			na.value = "grey"),
												 		scale_shape_manual=list(
												 			values = palettes$nodes$shape,
												 			na.value = 19),
												 		scale_edge_fill_manual=list(
												 			values = palettes$nodes$discrete,
												 			na.value = "grey",
												 			guide = "none"),
												 		geom_edge_fan=list(
												 			aes(color = {{edge_color}},
												 					# linewidth={{edge_color}},
												 					linetype = {{edge_linetype}}),
												 			arrow = arrow(length = unit(2, 'mm'),
												 										type = "open"),
												 			end_cap = circle(2.5, 'mm'),
												 			edge_alpha = edge_alpha,
												 			show.legend = FALSE
												 		),
												 		geom_edge_loop=list(
												 			aes(color = {{edge_color}}),
												 			arrow = arrow(length = unit(2, 'mm'),
												 										type = "open"),
												 			end_cap = circle(2.5, 'mm'),
												 			edge_alpha = edge_alpha
												 		),
												 		geom_edge_bundle_force2=list(
												 			aes(color = {{edge_color}},
												 					 linetype = {{edge_linetype}}),
												 			edge_alpha = edge_alpha
												 		),
												 		geom_edge_density=NULL,#list(ggplot2::aes(fill=predicate)),
												 		geom_node_point=list(
												 			aes(color = {{node_color}},
												 					shape = {{node_shape}},
												 					size = {{node_size}}),
												 			alpha = node_alpha
												 		),
												 		geom_node_label=list(
												 			aes(label = str_wrap(name, 20)),
												 			box.padding = 0.4,
												 			min.segment.length=0,
												 			size = 2,
												 			repel = TRUE,
												 			segment.colour = alpha("black",edge_alpha),
												 			segment.linetype = "dotted",
												 			fill = "#FFFFFF88"
												 		)
												 )
												 ) {
	if(isTRUE(bundle)){
		layer_args$geom_edge_fan <- NULL
		layer_args$geom_edge_loop <- NULL
	} else{
		layer_args$geom_edge_bundle_force2 <- NULL
	}
	if(edges(x)|>pull({{edge_color}})|>is.numeric()){
		layer_args$scale_edge_color_manual <- NULL
		layer_args$scale_edge_fill_manual <- NULL
	} else {
		layer_args$scale_edge_color_gradientn <- NULL
	}
	ggraph(x,
				 layout = layout,
				 ...) +
		list(
			## theme
			if(!is.null(layer_args$theme_bw)){
				do.call(ggplot2::theme_bw, layer_args$theme_bw)
			},
			if(!is.null(layer_args$theme)){
				do.call(ggplot2::theme, layer_args$theme)
			},
			## edge color
			if(!is.null(layer_args$scale_edge_color_manual)){
				do.call(ggraph::scale_edge_color_manual, layer_args$scale_edge_color_manual)
			},
			## edge fill
			if(!is.null(layer_args$scale_edge_fill_manual)){
				do.call(ggraph::scale_edge_fill_manual, layer_args$scale_edge_fill_manual)
			},
			## node color
			if(!is.null(layer_args$scale_color_manual)){
				do.call(ggplot2::scale_color_manual, layer_args$scale_color_manual)
			},
			## node shape
			if(!is.null(layer_args$scale_shape_manual)){
				do.call(ggplot2::scale_shape_manual, layer_args$scale_shape_manual)
			},
			## edges
			if(!is.null(layer_args$geom_edge_density)){
				do.call(ggraph::geom_edge_density, layer_args$geom_edge_density)
			},
			if(!is.null(layer_args$geom_edge_fan)){
				do.call(ggraph::geom_edge_fan, layer_args$geom_edge_fan)
			},
			if(!is.null(layer_args$geom_edge_loop)){
				do.call(ggraph::geom_edge_loop, layer_args$geom_edge_loop)
			},
			if(!is.null(layer_args$geom_edge_bundle_force2)){
				do.call(ggraph::geom_edge_bundle_force2, layer_args$geom_edge_bundle_force2)
			},
			if(!is.null(layer_args$scale_edge_color_gradientn)){
				do.call(ggraph::scale_edge_color_gradientn, layer_args$scale_edge_color_gradientn)
			},
			## nodes
			if(!is.null(layer_args$geom_node_point)){
				do.call(ggraph::geom_node_point, layer_args$geom_node_point)
			},
			if(!is.null(layer_args$geom_node_label)){
				do.call(ggraph::geom_node_label, layer_args$geom_node_label)
			}
		)
}
