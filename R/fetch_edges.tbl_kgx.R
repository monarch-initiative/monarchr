#' @export
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
expand.tbl_kgx <- function(graph, ...) {
    # check to see if g has a last_engine attribute
    if(!is.null(attr(graph, "last_engine"))) {
        engine <- attr(graph, "last_engine")
        return(expand(engine, graph, ...))
    } else {
        stop("Error: tbl_kgx object does not have a most recent engine. Use expand(engine, g, ...) instead.")
    }
}
