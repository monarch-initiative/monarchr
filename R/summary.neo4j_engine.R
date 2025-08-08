#' Summarize contents of a Neo4j KG engine
#'
#' Given a Neo4j based KG engine, provides summary information in the form of
#' node counts, category counts across nodes, relationship type counts, and available properties.
#' General information about the graph is printed to the console, and a list of
#' dataframes with this information is returned invisibly. Also returned
#' are `cats`, `preds`, and `props` entries, containing lists of available
#' categories/predicates/properties for convenient auto-completion in RStudio.
#'
#' @param object A `neo4j_engine` object
#' @param ... Other parameters (not used)
#' @param quiet Logical, whether to suppress printing of the summary
#' @return A list of dataframes and named lists
#' @export
#' @examplesIf monarch_engine_check()
#' # prints a readable summary and returns a list of dataframes
#' stats <- monarch_engine() |> summary()
#' print(stats)
#'
summary.neo4j_engine <- function(object, ..., quiet = FALSE) {
	if(!quiet) {
		cat("\n")
		cat("A Neo4j-backed knowledge graph engine.\n")
		cat("Gathering statistics, please wait...\n")
	}

    node_summary_df <- cypher_query_df(object, "MATCH (n) UNWIND labels(n) AS category WITH category, COUNT(n) AS count RETURN category, count ORDER BY count DESC")
    edge_summary_df <- cypher_query_df(object, "MATCH ()-[r]->() RETURN type(r) AS predicate, COUNT(*) AS count ORDER BY count DESC")

    counts_query <- "
	    // Count the total number of nodes
			MATCH (n)
			RETURN 'nodes_total' AS Type, COUNT(n) AS Count
			UNION
			// Count the total number of edges
			MATCH ()-[r]->()
			RETURN 'edges_total' AS Type, COUNT(r) AS Count
	    "

    total_df <- cypher_query_df(object, counts_query)
    total_nodes <- total_df$Count[1]
    total_edges <- total_df$Count[2]

    properties <- cypher_query_df(object, "CALL db.propertyKeys()")$propertyKey


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
        cat("Available node and edge properties:\n")
        print(properties)
        cat("\n\n")
        cat("For more information about Biolink node (Class) and edge (Association) properties, see https://biolink.github.io/biolink-model/.")
    }

    cats <- as.list(node_summary_df$category)
    names(cats) <- cats

    preds <- as.list(edge_summary_df$predicate)
    names(preds) <- preds

    props <- as.list(properties)
    names(props) <- props

    return(invisible(list(node_summary = node_summary_df,
    											edge_summary = edge_summary_df,
    											total_nodes = total_nodes,
    											total_edges = total_edges,
    											cats = cats,
    											preds = preds,
    											props = props)))
}
