#' Transfer information over edges to nodes.
#'
#' Used to 'transfer' information from nodes to other nodes across
#' specific predicates, either in an outward direction (along the edge
#' direction) or inward (against the edge direction). Returns a node-propery
#' column; intended to be used with mutate() on nodes.
#'
#' The return value will be either a list, or if the result would be a list
#' with all length-1 or length-0 elements, a vector with 0-length elements
#' replaced by NA. Practically, this results in a list when necessary and a vector
#' otherwise.
#'
#' @return Vector or list, with one entry per node.
#' @seealso [roll_up()], [transitive_closure], [descendants()], [ancestors()]
#'
#' @examples
#' engine <- file_engine(system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr"))
#'
#' engine |> fetch_nodes(name %~% "COL1A") |>
#' 	expand(categories = "biolink:Disease") |>
#' 	activate(nodes) |>
#' 	mutate(caused_by_genes = transfer(name, over = "biolink:causes", direction = "out")) |>
#' 	mutate(causes_diseases = transfer(name, over = "biolink:causes", direction = "in")) |>
#' 	plot.tbl_kgx(node_label = paste(name,
#' 																	" caused by: ", caused_by_genes,
#' 																	" causes: ", causes_diseases),
#' 							 label_size = 3)
#'
#' @import tidygraph
#' @export
transfer <- function(colname = NULL, over, direction = "out") {
	if(direction != "in" & direction != "out") {
		stop("Error, 'toward' must be one of 'in' or 'out'.")
	}
	from = ifelse(direction == "out", "subject", "object")
	toward = ifelse(from == "subject", "object", "subject")
	#print(paste("Pulling from", from, "to", toward, "data from", {{colname}}))

	edge_data <- .E() |> filter(predicate %in% over)
	node_data <- .N()
	# so we have a set of edges, for each unique object... we need to collect expr
	node_ids <- node_data$id
	agg_results <- lapply(node_ids, function(node_id) {
		relevant_edges <- edge_data[edge_data[[toward]] == node_id,] #|> filter(object == node_id)
		source_ids <- unique(relevant_edges[[from]])  # e.g. g1, g2, g3....
		relevant_subject_nodes <- node_data |> filter(id %in% source_ids)
		relevant_data <- relevant_subject_nodes |> pull({{colname}})
		relevant_data
	})

	# determines if the given list can be converted safely to a vector
	vec_safe <- function(lst) {
		lengths_ok <- all(lengths(lst) == 1 | lengths(lst) == 0)
		types <- sapply(lst, typeof)
		types_ok <- length(unique(types)) == 1
		lengths_ok && types_ok
	}

	if(vec_safe(agg_results)) {
		# length-0 lists need to be replaced with a single NA
		agg_results <- lapply(agg_results, function(el) {
			if(length(el) == 0) {
				NA
			} else {
				el
			}
		})
		agg_results <- unlist(agg_results)
	}

	return(agg_results)
}
