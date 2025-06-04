#' Compute transitive closure over a predicate.
#'
#' Computes the transitive closure of a graph, treating the specified
#' predicate as transitive. Resulting edge predicates will the be the
#' same, but have primary_knowledge_source set to transitive_<predicate>.
#'
#' @return Graph with transitive edges added.
#' @seealso [roll_up()], [transfer()], [descendants()], [ancestors()]
#'
#' @examples
#' engine <- file_engine(system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr"))
#'
#' engine |> fetch_nodes(name == "Tall stature") |>
#'   expand_n(predicates = "biolink:subclass_of", direction = "out", n = 3) |>  # get 2 levels of ancestors
#'   activate(edges) |>
#'   filter(primary_knowledge_source == "infores:upheno") |>
#'   transitive_closure(predicate = "biolink:subclass_of") |>
#'   plot(edge_color = primary_knowledge_source)
#'
#' @import tidygraph
#' @export
transitive_closure <- function(g, predicate = "biolink:subclass_of") {
	if(length(predicate) != 1) {
		stop("Error: predicate parameter of transitive_closure() must be length 1.")
	}
	# if there are no edges to close, return the input
	p <- predicate
	if(nrow(edges(g) |> filter(predicate == p)) == 0) {return(g)}

	active_tbl <- active(g)

	with_downstream <- g |>
		activate(nodes) |>
		mutate(downstream_nodes = roll_down(id, include_self = FALSE, predicate = predicate)) |>
		filter(!is.na(downstream_nodes))

	# create a new edge df... start by getting the nodes and the list col
	new_edges <- nodes(with_downstream) |>
		select(id, downstream_nodes) |>
		tidyr::unnest(downstream_nodes, keep_empty = FALSE) |>
		mutate(subject = id, predicate = predicate, object = downstream_nodes, primary_knowledge_source = paste0("transitive_", predicate)) |>
		select(subject, predicate, object, primary_knowledge_source) |>
		mutate(edge_key = paste(subject, "@", predicate, "@", object))

	node_indices <- nodes(g) |> mutate(index = dplyr::row_number()) |> select(id, index)

	# we create edge keys so that we can filter out edges that are duplicates created as part of
	# the process above (the original, non-transitive edges)
	g <- g |>
		activate(edges) |>
		mutate(edge_key = paste(subject, "@", predicate, "@", object))

	new_edges <- new_edges |>
		dplyr::left_join(node_indices, by = c("subject" = "id")) |>
		dplyr::rename(from = index) |>
		dplyr::left_join(node_indices, by = c("object" = "id")) |>
		dplyr::rename(to = index) |>
		mutate(edge_key = paste(subject, "@", predicate, "@", object)) |>
		filter(!edge_key %in% edges(g)$edge_key)

	res <- g |>
		tidygraph::bind_edges(new_edges) |>
		activate(edges) |>
		select(-edge_key) |>
		activate(!!rlang::sym(active_tbl))

	res
}
