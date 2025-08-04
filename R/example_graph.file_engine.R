#' Return an example set of nodes from a KG engine.
#'
#' Given a KGX file-based KG engine, returns a graph representing the diversity
#' of node categories and edge predicates for browsing. The returned graph is guaranteed to
#' contain at least one node of every category, and at least one edge of every
#' predicate. No other guarantees are made: the example graph is not minimal
#' to satisfy these criteria, it is not random or even pseudo-random, and it
#' may not be connected.
#'
#' @param engine A `file_engine` object
#' @param ... Other parameters (not used)
#' @return A tbl_kgx graph
#' @export
#' @examples
#' # Using example KGX file packaged with monarchr
#' data(eds_marfan_kg)
#'
#' # Retrieve and print an example graph:
#' g <- eds_marfan_kg |> example_graph()
#' print(g)
#' @import tidygraph
#' @import dplyr
example_graph.file_engine <- function(engine, ...) {
	# first, let's discover the different edge types (predicates) available
	edges_df <- engine$graph |>
		activate(edges) |>
		as.data.frame()

	nodes_df <- engine$graph |>
		activate(nodes) |>
		as.data.frame()


	pred_types <- edges_df |>
		pull(predicate) |>
		unique()

	# next we get a bunch of edges of the different predicate types as a graph
	sample_edges <- edges_df |>
		group_by(predicate) |>
		slice(1) |>
		ungroup() |>
		select(-to, -from)

	sample_nodes <- nodes_df |>
		filter(id %in% sample_edges$subject | id %in% sample_edges$object)

	sample_preds_graph <- tbl_kgx(nodes = sample_nodes, edges = sample_edges, attach_engine = engine)

	# this might not represent all categories however.
	# So we compute the categories that are represented thus far
	used_categories <- sample_preds_graph |>
		activate(nodes) |>
		as.data.frame() |>
		pull(category) |>
		unlist() |>
		unique()

	# get the available categories
	all_node_categories <- nodes_df |>
		pull(category) |>
		unlist() |>
		unique()


	# compute the node categories that are still needed
	needed_categories <- setdiff(all_node_categories, used_categories)

	# now sample nodes of those categories, and an arbitrary connection
	# trouble is, nodes_df$category is a list column...
	sample_cats_node_ids <- needed_categories |> lapply(function(cat) {
		has_cat_rows <- which(cat %in_list% nodes_df$category)
		return(nodes_df$id[has_cat_rows[1]])
		# unique because a single node may be selected to represent multiple needed categories
	}) |> unlist() |> unique()

	# ok, we have ids that cover the needed categories. Let's grab one row from
	# the edges table for each (id could be subject or object)
	sample_cats_edges_list <- sample_cats_node_ids |> lapply(function(node_id) {
		row <- edges_df |>
			filter(node_id == subject | node_id == object) |>
			head(n = 1) |>
			select(-to, -from)
	})
	sample_cats_edges <- do.call(rbind, sample_cats_edges_list)

	# now we need to select the corresponding nodes via their ids
	sample_cats_all_ids <- c(sample_cats_edges$subject, sample_cats_edges$object) |>
		unique()

	sample_cats_nodes <- nodes_df |>
		filter(id %in% sample_cats_all_ids)

	# and join it all up
	sample_cats_graph <- tbl_kgx(nodes = sample_cats_nodes, edges = sample_cats_edges, attach_engine = engine, )

	suppressMessages(all <- kg_join(sample_cats_graph, sample_preds_graph), classes = "message")
	return(all)
}
