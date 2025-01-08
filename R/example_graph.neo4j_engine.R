#' Return an example set of nodes from a KG engine.
#'
#' Given a KGX Neo4j KG engine, returns a graph representing the diversity
#' of node categories and edge predicates for browsing. The returned graph is guaranteed to
#' contain at least one node of every category, and at least one edge of every
#' predicate. No other guarantees are made: the example graph is not minimal
#' to satisfy these criteria, it is not random or even pseudo-random, and it
#' may not be connected.
#'
#' @param engine A `neo4j_engine` object
#' @param ... Other parameters (not used)
#' @return A tbl_kgx graph
#' @export
#' @examplesIf monarch_engine_check()
#' # Retrieve and print an example graph:
#' g <- monarch_engine() |> example_graph()
#' print(g)
#' @import tidygraph
#' @import dplyr
example_graph.neo4j_engine <- function(engine, ...) {
	# first, let's discover the different edge types (predicates) available from the schema info
	pred_types_query <- "CALL db.schema.visualization() YIELD relationships
                       UNWIND relationships AS rel
                       RETURN DISTINCT type(rel) AS predicate"
	pred_types <- cypher_query_df(engine, pred_types_query)

	# next we get a bunch of edges of the different predicate types as a graph
	sample_preds_query <- paste0("MATCH (a)-[r:`", pred_types$predicate, "`]->(b) RETURN a, b, r LIMIT 1")
	sample_preds_graph <- cypher_query(engine, query = sample_preds_query)

	# this might not represent all categories however.

	# So we compute the categories that are represented thus far
	used_categories <- sample_preds_graph |>
		activate(nodes) |>
		as.data.frame() |>
		pull(category) |>
		unlist() |>
		unique()

	# get the available categories from the schema
	categories_query <- "CALL db.labels() YIELD label RETURN DISTINCT label"
	all_node_categories <- cypher_query_df(engine, categories_query)$label

	# compute the node categories that are still needed
	needed_categories <- setdiff(all_node_categories, used_categories)

	# now sample nodes of those categories, and an arbitrary connection
	sample_cats_query <- paste0("MATCH (a:`", needed_categories, "`) -[r]- (b) RETURN a, r, b LIMIT 1")
	sample_new_cats <- cypher_query(engine, query = sample_cats_query)

	# finally, we join the two and return
	suppressMessages(full_sample <- kg_join(sample_preds_graph, sample_new_cats), classes = "message")

	return(full_sample)
}
