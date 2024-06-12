#' @export
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
fetch_edges.tbl_kgx <- function(graph, ...) {
    # check to see if g has a last_engine attribute
    if(!is.null(attr(graph, "last_engine"))) {
        engine <- attr(graph, "last_engine")
        return(fetch_edges(engine, graph, ...))
    } else {
        stop("Error: tbl_kgx object does not have a most recent engine. Use fetch_edges(engine, g, ...) instead.")
    }
}
