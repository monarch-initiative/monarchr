#' Summarize contents of a KGX-file-based KG engine
#'
#' Given a KGX file-based KG engine, provides summary information in the form of
#' node counts, category counts across nodes, and relationship type counts.
#' General information about the graph is printed to the console, and a list of
#' dataframes describing node and edge counts is returned invisibly. Also returned
#' are `cats` and `preds` entries, containing lists of available node categories and
#' edge predicates, respectively, for convenient auto-completion in RStudio.
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
    }


    cats <- as.list(node_summary_df$category)
    names(cats) <- cats

    preds <- as.list(edge_summary_df$predicate)
    names(preds) <- preds

    return(invisible(list(node_summary = node_summary_df,
    											edge_summary = edge_summary_df,
    											total_nodes = total_nodes,
    											total_edges = total_edges,
    											cats = cats,
    											preds = preds)))
}
