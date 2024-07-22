#' Summarize neighborhood
#'
#' Summarizes the neighborhood of the nodes of a given graph. Specifically,
#' letting $N$ be the set of nodes in the given graph, this function retrieves
#' counts of relationship predicates (or node categories) of nodes connected to
#' $N$ but not in $N$. This can be useful to examine the scale and scope of a
#' graph's collective neighborhood in the larger KG.
#'
#' Note that the number of relationships returned may be larger than the number
#' of nodes they connect to; use summarize = "edges" to see edge counts between
#' nodes of different categories, and summarize = "nodes" to see counts of connected
#' node categories.
#' 
#' Additionally, when using `summarize = "edges"`, the summary will include 
#' edges that may already be present in the query graph.
#'
#' It is also possible to specify the direction of edges to include in the
#' neighborhood, using the direction parameter. The default is "both", which
#' includes both incoming and outgoing edges.
#'
#'
#' @param graph A query graph to summarize the surrounding neighborhood for
#' @param engine (Optional) An engine to use. If not provided, the graph's most recent engine is used.
#' @param direction The direction of edges to include in the neighborhood
#' @param summarize Whether to summarize edges or nodes (default "edges")
#'
#' @return A tbl_kgx graph
#'
#' @export
#' @examplesIf monarch_engine_check()
#' g <- monarch_search("fanconi anemia", limit = 5) |>
#'   summarize_neighborhood(direction = "both", summarize = "edges")
#'
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood <- function(graph, engine = NULL, direction = "both", summarize = "edges") {
    UseMethod("summarize_neighborhood")
}