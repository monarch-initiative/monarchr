#' Get most recent engine from a graph.
#'
#' Given a tbl_kgx graph, retrieve the last-used engine.
#'
#' @param g A tbl_kgx graph.
#' @param fail_if_missing If TRUE, fail if there is no engine associated with the graph.
#' @return A graph engine object.
#' @examples
#' # Using example KGX file packaged with monarchr
#' data(eds_marfan_kg)
#'
#' g <- eds_marfan_kg |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' print(get_engine(g))
#'
#' @export
get_engine.tbl_kgx <- function(g, fail_if_missing = TRUE) {
	engine <- attr(g, "last_engine")
	if (is.null(engine) && fail_if_missing) {
		stop("No engine associated with this graph. Unable to proceed.")
	}
	return(engine)
}
