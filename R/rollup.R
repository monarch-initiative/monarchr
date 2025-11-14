
#' Internal function for rolling up and down
#'
#'
#' @param column The node column to draw rollup or rolldown information from.
#' @param fun The aggregation function to use when rolling up or down. Default is `c`
#' @param include_self Whether to include each nodes' value in `column` in the rollup/rolldown for that node.
#' @param predicates A vector of relationship predicates (nodes in g are subjects in the KG), indicating which edges to consider in the rollup/rolldown. Should be transitive; default `biolink:subclass_of`
#' @param direction Whether to roll up or down.
#' @param ... Other parameters (unused)
#'
#' @importFrom igraph ego
#' @importFrom igraph V
#' @import tidygraph
#' @import dplyr
roll <- function(  column,
									 fun = c,
									 include_self = TRUE,
									 predicates = "biolink:subclass_of",
									 direction = "up",
									 ...) {

	g <- .G()

	g_filt <- g |>
		activate(edges) |>
		filter(predicate %in% predicates)

	if(direction != "up" & direction != "down") {
		stop("Error, 'direction' must be one of 'up' or 'down'.")
	}

	# for some reason usin Inf or -1 to use an infinite neighborhood size
	# works on my local machine, but not in the github build checks.
	# using order = num_nodes + 1 to ensure the order is large enough
	num_nodes = nrow(nodes(g_filt)) + 1

	mode = ifelse(direction == "up", "in", "out")

	descendants_list <- ego(
		graph = as.igraph(g_filt),
		order = num_nodes,
		nodes = V(g_filt),
		mode = mode,
		mindist = 1
	)
	# at this point each element is a set of nodes;

	if(include_self) {
		# Include self in each set (if not already included)
		descendants_list <- lapply(seq_along(descendants_list), function(i) {
			unique(c(i, descendants_list[[i]]))
		})
	} else {
		descendants_list <- lapply(seq_along(descendants_list), function(i) {
			unique(as.integer(descendants_list[[i]]))
		})
	}

	# at this point the result is a list, mapping node index to a vector of descendant
	# node indices

	## 3 – pull the column we’re going to aggregate -----------------------------
	node_vals <- nodes(g_filt) |> pull({{column}})

	## 4 – apply `fun` to each set of descendants ------------------------------
	# not using sapply here, as it can return a matrix or array, which can't become a column
	agg_results <- lapply(descendants_list, function(idx) {
			fun(node_vals[idx])    # wrap in list → always a list-column
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



#' Roll node data up (along) or down (against) transitive edges. Use with `mutate()`
#'
#' This function computes, for each node, an aggregated set of data from all
#' descendant (for roll-ups) or ancestor (for roll-downs) nodes defined by
#' specified edge predicates. Designed for use with `mutate()`
#' on node data, for each node N, the specified `fun` is called
#' on the node table `column` filtered to nodes that can reach (be reached by) N over
#' `predicates` edges. If `include_self` is true, N itself is included.
#'
#' Note that path counts and order are not considered; rollups (rolldowns)
#' collect information from all descendant (ancestor) nodes as a set.
#'
#' The return value will be either a list, or if the result would be a list
#' with all length-1 or length-0 elements, a vector with 0-length elements
#' replaced by NA. Practically, this results in a list when necessary and a vector
#' otherwise.
#'
#' @inheritParams roll
#' @return Vector or list, with one entry per node.
#' @seealso [roll_down()], [descendants()], [ancestors(), [transfer()], [transitive_closure()]]
#'
#' @examples
#' data(eds_marfan_kg)
#'
#' eds_marfan_kg |>
#'   fetch_nodes(name == "Tall stature" | name == "Short stature") |>
#'   # get 2 levels of ancestors
#'   expand_n(predicates = "biolink:subclass_of", direction = "out", n = 2) |>
#'   activate(nodes) |>
#'   # random count value per node
#'   mutate(count = rpois(graph_order(), 1.5)) |>
#'   # apply sum to descendant (and self) values
#'   mutate(sum_count = roll_up(count, fun = sum, include_self = TRUE)) |>
#'   plot(node_label = paste(name, " count: ", count, "sum_count: ", sum_count))
#'
#' @name rolling
#' @export
roll_up <- function(column = NULL, fun = c, include_self = TRUE, predicates = "biolink:subclass_of", ...) {
	roll({{column}}, fun, include_self, predicates, direction = "up")
}

#' @rdname rolling
#' @export
roll_down <- function(column = NULL, fun = c, include_self = TRUE, predicates = "biolink:subclass_of", ...) {
	roll({{column}}, fun, include_self, predicates, direction = "down")
}

