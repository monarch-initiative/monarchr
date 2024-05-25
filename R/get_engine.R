# given a tbl_kgx graph, retrieve the last-used engine
# if there is no engine, fail if fail_if_missing is TRUE (default)

#' Get engine
#' 
#' Given a tbl_kgx graph, retrieve the last-used engine.
#' 
#' @param g A tbl_kgx graph.
#' @param fail_if_missing If TRUE, fail if there is no engine associated with the graph.
#' @return A graph engine object.
#' @examples
#' g <- monarch_search("fanconi anemia", limit = 5)
#' engine <- get_engine(g)
#' @export
get_engine <- function(g, fail_if_missing = TRUE) {
    engine <- attr(g, "last_engine")
    if (is.null(engine) && fail_if_missing) {
        stop("No engine associated with this graph. Unable to proceed.")
    }
    return(engine)
}