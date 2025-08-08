#' Summarize contents of a KGX-file-based KG engine
#'
#' Given a KGX file-based KG engine, provides summary information in the form of
#' node counts, category counts across nodes, relationship type counts, and available properties.
#' General information about the graph is printed to the console, and a list of
#' dataframes with this information is returned invisibly. Also returned
#' are `cats`, `preds`, and `props` entries, containing lists of available
#' categories/predicates/properties for convenient auto-completion in RStudio.
#'
#' When applied to a `file_engine`, also included are node-specific and edge-specific properties.
#'
#' @param object A `file_engine` object
#' @param ... Other parameters (not used)
#' @param quiet Logical, whether to suppress printing of the summary
#' @return A list of dataframes and named lists
#' @export
#' @examples
#' # Using example KGX file packaged with monarchr
#' data(eds_marfan_kg)
#'
#' # prints a readable summary and returns a list of dataframes
#' res <- eds_marfan_kg |> summary()
#' print(res)
#' @import tidygraph
#' @import dplyr
summary.file_engine <- function(object, ..., quiet = FALSE) {
	if(!quiet) {
		cat("\n")
		cat("A KGX file-backed knowledge graph engine.\n")
	}

    g <- object$graph

    total_nodes <- g |>
        activate(nodes) |>
    	  as.data.frame() |>
        nrow()

    node_props <- g |>
      activate(nodes) |>
      as.data.frame() |>
      colnames()

    edge_props <- g |>
      activate(edges) |>
      as.data.frame() |>
      colnames()

    nodes_df <- nodes(g)
    edges_df <- edges(g)


    node_prop_counts <- lapply(node_props, function(prop_name) {
      count <- length(nodes_df[[prop_name]][!is.na(nodes_df[[prop_name]])])
      data.frame(property = prop_name, count = count)
    })
    node_prop_counts <- do.call(rbind, node_prop_counts)
    node_prop_counts <- node_prop_counts[rev(order(node_prop_counts$count)),]


    edge_prop_counts <- lapply(edge_props, function(prop_name) {
      count <- length(edges_df[[prop_name]][!is.na(edges_df[[prop_name]])])
      data.frame(property = prop_name, count = count)
    })
    edge_prop_counts <- do.call(rbind, edge_prop_counts)
    edge_prop_counts <- edge_prop_counts[rev(order(edge_prop_counts$count)),]

    properties <- unique(c(node_props, edge_props))

    total_edges <- g |>
        activate(edges) |>
    	  as.data.frame() |>
        nrow()

    all_node_cats <- g |>
    	activate(nodes) |>
    	as.data.frame() |>
    	pull(category) |>
    	unlist() |>
    	sort() |>
    	rle()

    node_summary_df <- data.frame(category = all_node_cats$values,
                                  count = all_node_cats$lengths) |>
                       arrange(desc(count))

    all_edge_predicates <- g |>
    	activate(edges) |>
    	as.data.frame() |>
    	pull(predicate) |>
    	sort() |>
    	rle()

    edge_summary_df <- data.frame(predicate = all_edge_predicates$values,
                                  count = all_edge_predicates$lengths) |>
                       arrange(desc(count))


    if(!quiet) {
    	cat("Total nodes: ", total_nodes, "\n")
    	cat("Total edges: ", total_edges, "\n")
    	cat("\n")
    	cat("Node category counts:\n")
    	# print the data frame without row names
    	print(node_summary_df, row.names = FALSE)
    	cat("\n")
    	cat("Edge type counts:\n")
    	# print the data frame without row names
    	print(edge_summary_df, row.names = FALSE)
      cat("\n")
      cat("Node property counts:\n")
      print(node_prop_counts, row.names = FALSE)
      cat("\n")
      cat("Edge property counts:\n")
      print(edge_prop_counts, row.names = FALSE)
      cat("\n\n")
      cat("For more information about Biolink node (Class) and edge (Association) properties, see https://biolink.github.io/biolink-model/.")
    }


    cats <- as.list(node_summary_df$category)
    names(cats) <- cats

    preds <- as.list(edge_summary_df$predicate)
    names(preds) <- preds

    props <- as.list(properties)
    names(props) <- props

    node_props <- as.list(node_props)
    names(node_props) <- node_props

    edge_props <- as.list(edge_props)
    names(edge_props) <- edge_props

    return(invisible(list(node_summary = node_summary_df,
    											edge_summary = edge_summary_df,
    											total_nodes = total_nodes,
    											total_edges = total_edges,
                          node_properties_summary = node_prop_counts,
                          edge_properties_summary = edge_prop_counts,
    											cats = cats,
                          preds = preds,
                          props = props)))
}
