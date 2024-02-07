#' Retrieve the neighborhood of nodes in a graph from the larger KG.
#' 
#' @param g A graph
#' @param query_categories A vector of node categories, indicating which nodes in g to consider in the query. If NULL (default), all nodes in g are considered.
#' @param predicates A vector of relationship predicates, indicating which edges to consider in the neighborhood. If NULL (default), all edges are considered.
#' @param result_categories A vector of node categories, indicating which nodes in the larger KG to consider as potential result nodes. If NULL (default), all nodes in the larger KG are considered as potential nodes.
#' @param direction The direction of edges to include in the neighborhood. If "out", predicates are only considered if source nodes in g are subjects in the relationship. If "in", predicates are only considered if source nodes in g are objects in the relationship. If "both" (default), predicates are considered regardless of the direction of the relationship.
