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
#' It is also possible to specify the direction of edges to include in the
#' neighborhood, using the direction parameter. The default is "both", which
#' includes both incoming and outgoing edges.
#'
#'
#' @param g A graph
#' @param direction The direction of edges to include in the neighborhood
#' @param summarize Whether to summarize edges, nodes, or both
#'
#' @return A tbl_kgx graph
#'
#' @export
#' @examples
#' g <- monarch_search("fanconi anemia")
#' summarize_neighborhood(g, summarize = "edges") # default
#' summarize_neighborhood(g, summarize = "nodes")
#'
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
summarize_neighborhood <- function(g, direction = "both", summarize = "edges") {
    assert_that(summarize %in% c("edges", "nodes"))
    assert_that(direction %in% c("both", "in", "out"))
    e <- get_engine(g)
    if (summarize == "edges") {
        return(summarize_neighborhood_edges(e, g, direction))
    } else if (summarize == "nodes") {
        return(summarize_neighborhood_nodes(e, g, direction))
    }
}
