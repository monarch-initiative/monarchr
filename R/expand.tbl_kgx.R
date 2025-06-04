#' @export
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
expand.tbl_kgx <- function(graph, ...) {
    # check to see if g has a last_engine attribute
	  active_tbl <- active(graph)
    if(!is.null(attr(graph, "last_engine"))) {
        engine <- attr(graph, "last_engine")
        if(any(c("monarch_engine", "neo4j_engine") %in% class(engine))) {
        	res <- expand_neo4j_engine(engine, graph, ...) |> activate(!!rlang::sym(active_tbl))
        	return(res)
        } else if("file_engine" %in% class(engine)) {
        	res <- expand_file_engine(engine, graph, ...) |> activate(!!rlang::sym(active_tbl))
        	return(res)
        } else {
        	stop("Error: unknown or incompatible engine.")
        }
        # return(expand(engine, graph, ...)) # this shouldn't be reachable
    } else {
        stop("Error: tbl_kgx object does not have a most recent engine.")
    }
}
