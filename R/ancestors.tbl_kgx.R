#' Expand ancestors transitively
#'
#' Expand a tbl_kgx graph to include all ancestors of nodes defined transitively
#' by "biolink:subclass_of" relationships. This is a simple wrapper around
#' expand(predicates = "biolink:subclass_of", direction = "out", transitive = TRUE)
#'
#'
#' @param g A `tbl_kgx()` graph to expand.
#' @param ... Other parameters (unused).
#'
#' @return A tbl_kgx graph.
#' @export
#' @examples
#' engine <- file_engine(system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr"))
#' g <- engine |>
#' 	 fetch_nodes(query_ids = "MONDO:0020066") |>
#' 	 ancestors()
#'
#' @import tidygraph
#' @import dplyr
ancestors.tbl_kgx <- function(g, ...) {
	return(g |> expand(predicates = "biolink:subclass_of",
										 direction = "out",
										 transitive = TRUE))
}
