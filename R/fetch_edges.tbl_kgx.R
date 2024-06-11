#' @export
#' @rdname fetch_edges
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
fetch_edges.tbl_kgx <- function(g, ...) {
    # check to see if g has a last_engine attribute
    if(!is.null(attr(g, "last_engine"))) {
        engine <- attr(g, "last_engine")
        return(fetch_edges(engine, g, ...))
    } else {
        stop("Error: tbl_kgx object does not have a most recent engine. Use fetch_edges(engine, g, ...) instead.")
    }
}
