#' Return an example set of nodes from a KG engine.
#'
#' Given a KG engine, returns a graph representing the diversity
#' of node categories and edge predicates for browsing. The returned graph is guaranteed to
#' contain at least one node of every category, and at least one edge of every
#' predicate. No other guarantees are made: the example graph is not minimal
#' to satisfy these criteria, it is not random or even pseudo-random, and it
#' may not be connected.
#'
#' @param engine A KG engine object
#' @param ... Other parameters (not used)
#' @return A tbl_kgx graph
#' @export
#' @examples
#' # Using example KGX file packaged with monarchr
#' data(eds_marfan_kg)
#'
#' # prints a readable summary and returns a list of dataframes
#' g <- eds_marfan_kg |> example_graph()
#' print(g)
#'
#' @examplesIf monarch_engine_check()
#' # prints a readable summary and returns a list of dataframes
#' g <- monarch_engine() |> example_graph()
#' print(g)
#' @import tidygraph
#' @import dplyr
example_graph <- function(engine, ...) {
	UseMethod("example_graph")
}

