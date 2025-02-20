###### Internal functions

# Generates a palette with num_colors entries, mapping
# inputs pseudorandomly to them. if levels_only = FALSE,
# a vector of RGB values the same length as input is returned.
# if levels_only = TRUE, a named vector is returned mapping input
# levels to RGB values
color_cats <- function(input, num_colors = 16, levels_only = TRUE) {
	# Ensure the input is treated as a factor
	factors <- factor(input)

	palette <- grDevices::hcl.colors(num_colors, palette = "Set3")


	# Hash function to convert factor levels to numeric values consistently
	hashes <- lapply(levels(factors), function(x) digest::digest(paste0(x, "salt"), algo = "crc32", serialize = FALSE))
	hash_integers <- sapply(hashes, function(x) strtoi(substr(x, 1, 5), base=16))

	# Map hashes to indices in the color palette
	# Use modulo to wrap around if there are more factors than colors
	color_indices <- (hash_integers %% length(palette)) + 1

	color_map <- setNames(palette[color_indices], levels(factors))
	if(levels_only) {
		return(color_map)
	} else {
		# Return the colors corresponding to the input
		return(unname(color_map[as.character(input)]))
	}
}


#' Send a graph to Cytoscape
#'
#' Given a tbl_kgx graph, send it to Cytoscape for visualization. Node labels
#' are mapped to node `name` (if available, otherwise they default to node `id`),
#' node color is mapped to `pcategory`, edge color is mapped to `predicate`,
#' node hover-over text is set to `description` (if available, otherwise node `id`),
#' and edge hover-over text is set to `predicate`. Nodes are layed out
#' using the Kamada-Kawai method. These properties and more may be customized in
#' the Cytoscape application. This function requires that Cytoscape is installed
#' and running independently of the R session.
#'
#'
#' @param g A `tbl_kgx()` graph to visualize.
#' @param ... other parameters passed to RCy3 functions, e.g. `base.url`.
#'
#' @return NULL, invisibly
#' @export
#' @examplesIf FALSE
#' engine <- file_engine(system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr"))
#' g <- engine |>
#' 	 fetch_nodes(query_ids = "MONDO:0020066") |>
#' 	 expand(predicates = "biolink:subclass_of", direction = "in", transitive = TRUE) |>
#' 	 expand(categories = c("biolink:PhenotypicFeature", "biolink:Gene"))
#'
#' # Cytoscape must be installed and running
#' cytoscape(g)
#'
#' @import RCy3
#' @import tidygraph
#' @import dplyr
cytoscape.tbl_kgx <- function(g, ...) {
	message("Sending data to Cytoscape and formatting; please wait.")
	tryCatch({
		RCy3::cytoscapePing(...)
	}, error = function(e) {
		message(paste0("Unable to connect to Cytoscape. Is it installed and running?", "\nRCy3 Error:\n", e$message))
	})

	nodes_df <- nodes(g)
	if("description" %in% colnames(nodes_df)) {
		nodes_df$desc_wrapped <- stringr::str_wrap(nodes_df$description, 50)
	} else {
		nodes_df$desc_wrapped <- nodes_df$id
	}

	edges_df <- edges(g)

	RCy3::createNetworkFromDataFrames(nodes_df,
																		edges_df,
																		title = "KG Nodes",
																		collection = "monarchr Graphs",
																		source.id.list = 'subject',
																		target.id.list = 'object',
																		...)
	RCy3::layoutNetwork('kamada-kawai', ...)

	pal <- color_cats(nodes(g)$pcategory, levels_only = TRUE)
	pal_edges <- color_cats(edges(g)$predicate, levels_only = TRUE)
	RCy3::setNodeColorMapping('pcategory', table.column.values = names(pal), colors = pal, mapping.type = 'd', ...)
	RCy3::setEdgeColorMapping('predicate', table.column.values = names(pal_edges), colors = pal_edges, mapping.type = 'd', ...)

	RCy3::setNodeTooltipMapping(table.column = 'desc_wrapped', ...)
	RCy3::setEdgeTooltipMapping(table.column = 'predicate', ...)
	RCy3::matchArrowColorToEdge(TRUE, ...)
	RCy3::setEdgeTargetArrowShapeDefault('ARROW', ...)
	return(invisible())
}
