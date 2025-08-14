#' Set the engine for a graph. See warning in details.
#'
#' Sets a given graph's engine to a given engine object and returns the graph.
#' WARNING: changing the backing engine for a graph dynamically is not
#' yet fully supported or tested.
#'
#' @param g A tbl_kgx graph.
#' @param engine An engine object.
#' @return A tbl_kgx graph.
#' @examples
#' # Using example KGX file packaged with monarchr
#' data(eds_marfan_kg)
#'
#' g <- eds_marfan_kg |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' other_engine <- eds_marfan_kg # this could be a different file engine (see `file_engine()`)
#' g <- set_engine(g, other_engine)
#'
#' print(get_engine(g))
#'
#' @export
set_engine <- function(g, engine) {
	attr(g, "last_engine") <- engine
	return(g)
}



