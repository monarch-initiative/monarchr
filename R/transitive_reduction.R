#' Compute transitive reduction over a predicate.
#'
#' Computes the transitive reduction of a graph, treating the specified
#' predicate as transitive.
#'
#' @return Graph with transitive edges added.
#' @seealso [transitive_closure()], [roll_up()], [transfer()], [descendants()], [ancestors()]
#'
#' @examples
#' engine <- file_engine(system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr"))
#'
#' g <- engine |> fetch_nodes(name == "Tall stature") |>
#' 	expand_n(predicates = "biolink:subclass_of", direction = "out", n = 3) |>
#' 	bind_edges(data.frame(from = 2, to = 9, predicate = "biolink_subclass_of", primary_knowledge_source = "hand_annotated"))
#'
#' plot(g, edge_color = primary_knowledge_source)
#'
#' g_closed <- g |>
#' 	transitive_closure(predicate = "biolink:subclass_of")
#'
#' plot(g_closed, edge_color = primary_knowledge_source)
#'
#' g_reduced <- g_closed |>
#' 	transitive_reduction()
#'
#' plot(g_reduced, edge_color = primary_knowledge_source)
#' @import tidygraph
#' @import dplyr
#' @importFrom sets as.set
#' @importFrom relations endorelation
#' @importFrom relations relation_incidence
#' @export
transitive_reduction <- function(g, predicate = "biolink:subclass_of") {
	# first we make a copy
	active_tbl <- active(g)
	g2 <- g

	# in the original, remove the predicate edges
	g <- g |>
		activate(edges) |>
		filter(predicate != predicate)

	df <- g2 |> activate(edges) |> as.data.frame()
	r <- endorelation(
		domain = lapply(unique(unlist(df[c("from", "to")])), sets::as.set),
		graph = df[c("from", "to")]
	)
	mat <- relation_incidence(relations::transitive_reduction(r))

	keep_edges <- which(mat == 1, arr.ind = TRUE) |>
		as.data.frame() |>
		rename(from = row, to = col)

	g_reduced <- g2 |>
		activate(edges) |>
		semi_join(keep_edges, by = c("from", "to"))

	# merge the original w g_reduced, adding back just the reduction edges
	suppressMessages(g <- kg_join(g, g_reduced), classes = "message") # suppress joining info
	g <- g |> activate(!!rlang::sym(active_tbl))

	return(g)
}
