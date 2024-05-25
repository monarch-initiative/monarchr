#' Summarize neighborhood nodes
#' 
#' This is a generic function to summarize neighborhood nodes. It is used by the
#' 'summarize_neighborhood' function to summarize the neighborhood nodes of the
#' input object.
#' 
#' @param obj An object of class 'neighborhood'.
#' @param ... Additional arguments to be passed to methods.
#' 
#' @examples
#' g <- monarch_search("fanconi anemia")
#' summarize_neighborhood_nodes(g)
#' 
#' @rdname summarize_neighborhood_nodes
summarize_neighborhood_nodes <- function(obj, ...) {
    UseMethod("summarize_neighborhood_nodes")
}