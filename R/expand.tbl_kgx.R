#' @export
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
expand.tbl_kgx <- function(graph, ...) {
    # check to see if g has a last_engine attribute
    if(!is.null(attr(graph, "last_engine"))) {
        engine <- attr(graph, "last_engine")
        if(any(c("monarch_engine", "neo4j_engine") %in% class(engine))) {
        	return(expand_neo4j_engine(engine, graph, ...))
        } else if("file_engine" %in% class(engine)) {
        	return(expand_file_engine(engine, graph, ...))
        } else {
        	stop("Error: unknown or incompatible engine.")
        }
        return(expand(engine, graph, ...))
    } else {
        stop("Error: tbl_kgx object does not have a most recent engine.")
    }
}
