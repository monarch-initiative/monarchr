# generic method for summarize_neighborhood_edges

#' Summarize neighborhood edges
#'
#' Summarizes the relationships in the neighborhood of a given graph. Specifically,
#' this function retrieves counts of different predicates connecting nodes in the given graph to
#' nodes in the larger KG. Counts are broken down by source and target node categories.
#' Note that the number of relationships returned may be larger than the number of nodes they connect to.
#'
#' @param e A graph engine object (optional - the recent engine used by the graph will be used if not supplied)
#' @param g A graph
#' @param direction The direction of edges to include in the neighborhood
#'
#' @return A tbl_kgx graph
#'
#' @export
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
#'
#' @examplesIf monarch_engine_check()
#' g <- monarch_engine() |>
#'   search_nodes("fanconi anemia", limit = 5) |>
#'   summarize_neighborhood_edges(direction = "both")
#'
summarize_neighborhood_edges <- function(obj, ...) {
    UseMethod("summarize_neighborhood_edges")
}
