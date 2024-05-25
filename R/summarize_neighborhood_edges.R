# generic method for summarize_neighborhood_edges

#' Summarize neighborhood edges
#'
#' Summarizes the relationships in the neighborhood of a given graph. Specifically,
#' this function retrieves counts of different predicates connecting nodes in the given graph to
#' nodes in the larger KG. Counts are broken down by source and target node categories.
#' Note that the number of relationships returned may be larger than the number of nodes they connect to.
#'
#' @param e A graph engine object
#' @param g A graph
#' @param direction The direction of edges to include in the neighborhood
#'
#' @return A tbl_kgx graph
#'
#' @examples
#' e <- neo4j_engine()
#' g <- monarch_search("fanconi anemia")
#' summarize_neighborhood_edges(g, direction = "both")
#'
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood_edges <- function(obj, ...) {
    UseMethod("summarize_neighborhood_edges")
}