# generic method for summarize_neighborhood_edges

#' Summarize neighborhood nodes
#'
#' Summarizes the nodes in the neighborhood of a given graph.
#'
#' @param e A graph engine object (optional - the recent engine used by the graph will be used if not supplied)
#' @param g A graph
#' @param direction The direction of edges to follow in the neighborhood
#'
#' @return A tbl_kgx graph
#'
#' @export
#'
#' @examplesIf monarch_engine_check()
#' g <- monarch_engine() |>
#'   search_nodes("fanconi anemia", limit = 5) |>
#'   summarize_neighborhood_nodes(direction = "both")
#'
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood_nodes <- function(obj, ...) {
    UseMethod("summarize_neighborhood_nodes")
}
