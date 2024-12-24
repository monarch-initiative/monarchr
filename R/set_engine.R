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
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#'
#' g <- file_engine(filename) |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' other_engine <- file_engine(filename) # this could be a different filename
#' g <- set_engine(g, other_engine)
#'
#' print(get_engine(g))
#'
#' @export
set_engine <- function(g, engine) {
	UseMethod("set_engine")
}
