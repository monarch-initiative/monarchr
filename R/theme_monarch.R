#' Monarch Initiative plot theme
#'
#' A \pkg{ggplot2}/\pkg{ggraph} theme inspired by the
#' \href{https://monarchinitiative.org/}{Monarch Initative}.
#' @param palettes A named list of discrete/continuous palettes for nodes/edges
#'  Default is \code{monarch_palettes}.
#' @param layer_args A named list of arguments for each \link{ggplot} layer.
#' @returns \pkg{ggplot2}/\pkg{ggraph} object.
#'
#' @export
#' @examples
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#' g <- file_engine(filename) |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")|>
#'           expand(categories = "biolink:Gene")
#' plot(g, layout="kk") + theme_monarch()
theme_monarch <- function(palettes = monarch_palettes(),
													layer_args=list(
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
												  	scale_color_manual=list(
												  		values = palettes$nodes$discrete,
												  		na.value = "grey"),
												  	scale_shape_manual=list(
												  		values = palettes$nodes$shape,
												  		na.value = 19),
												  	scale_edge_fill_manual=list(
												  		values = palettes$edges$discrete,
												  		na.value = "grey",
												  		guide = "none")
												  	)){
	list(
		## theme
		if(!is.null(layer_args$theme_bw)) do.call(ggplot2::theme_bw, layer_args$theme_bw),
		if(!is.null(layer_args$theme)) do.call(ggplot2::theme, layer_args$theme),
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
		}
	)
}

