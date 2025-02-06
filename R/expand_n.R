#' Iteratively fetch additional knowledge graph edges connected to a query graph
#'
#' Given an initialized \link{tbl_kgx} graph, iteratively expand the graph
#' \code{n} iterations using certain predicates/categories.
#' Arguments can either be a single value or a list of values.
#' If an argument is provided as a list, its length must be equal to the number
#' of iterations (\code{n}).
#' @param return_each If TRUE, return a list of graphs for each iteration.
#' If FALSE, return the final graph with all expanded edges.
#' @param n Number of expansion iterations to run.
#' @inheritParams expand
#' @param transitive NULL (not used in this function).
#'
#' @return A `tbl_kgx()` graph
#' @export
#' @examples
#' ## Using example KGX file packaged with monarchr
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#' g <- file_engine(filename) |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")
#' g_expanded <- g |>
#'               expand_n(predicates = "biolink:subclass_of", n=3)
#' @import tidygraph
#' @import dplyr
expand_n <- function(graph,
										 return_each = FALSE,
										 direction = "both",
										 predicates = NULL,
										 categories = NULL,
										 transitive = NULL,
										 drop_unused_query_nodes = FALSE,
										 n=1,
										 ...) {
	## Check args
	## Check args
	check_len <- function(arg,n,i){
		if(is.list(arg)){
			if(length(arg) != n){
				stop(paste("When provided a list, arguments must be equal to n."))
			}
			return(arg[[i]])
		}else{
			return(arg)
		}
	}
	if(!is.null(transitive)) {
		warning("Arguments to expand_n() are passed on to expand(), except for transitive which is set to NULL. Ignoring provided setting for transitive in expand_n().")
	}

	## Expand graph
	message(paste(
		"Initial graph size:",
		nrow(nodes(graph)),"nodes ||",nrow(edges(graph)),"edges"
	))
	if(return_each) graph_list <- list(iteration0=graph)

	for(i in 1:n){
		message("Expanding graph: iteration ",i,"/",n)
		graph <- expand(graph = graph,
										direction = check_len(direction,n,i),
										predicates = check_len(predicates,n,i),
										categories = check_len(categories,n,i),
										transitive = NULL,
										drop_unused_query_nodes = check_len(drop_unused_query_nodes,n,i),
										...)
		if(return_each) graph_list[[paste0("iteration",i)]] <- graph
		message(paste(
			"Graph size:",
			nrow(nodes(graph)),"nodes ||",nrow(edges(graph)),"edges"
		))
	}
	if(return_each){
		return(graph_list)
	} else {
		return(graph)
	}
}
