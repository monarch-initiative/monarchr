#' @export
#' @importFrom tidygraph as_tibble
nodes.tbl_kgx <- function(x, ...) {
  tidygraph::as_tibble(x, active = "nodes")
}


#' @export
#' @importFrom tidygraph as_tibble
edges.tbl_kgx <- function(x, ...) {
  tidygraph::as_tibble(x, active = "edges")
}


#' Get graph nodes table.
#'
#' @param g A graph object
#' @return A tibble with the nodes of the graph
#' @importFrom tidygraph as_tibble
#' @export
#' @examples
#' # (using the MONDO KGX file packaged with monarchr)
#' filename <- system.file("extdata", "mondo_kgx_tsv.tar.gz", package = "monarchr")
#'
#' g <- file_engine(filename) |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' print(nodes(g))
nodes <- function(g, ...) {
  UseMethod("nodes")
}

#' Get graph edges table.
#'
#' @param g A tbl_kgx graph.
#' @return A tibble with the edges of the graph
#' @importFrom tidygraph as_tibble
#' @param x Input graph
#' @param ... Other options (unused)
#' @export
#' @examples
#' # (using the MONDO KGX file packaged with monarchr)
#' filename <- system.file("extdata", "mondo_kgx_tsv.tar.gz", package = "monarchr")
#'
#' g <- file_engine(filename) |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' print(edges(g))
edges <- function(g, ...) {
  UseMethod("edges")
}

#' Explode a graph into a list of single-node graphs
#'
#' @param g A tbl_kgx graph.
#' @return A list of tbl_kgx graphs.
#' @examples
#' # (using the MONDO KGX file packaged with monarchr)
#' filename <- system.file("extdata", "mondo_kgx_tsv.tar.gz", package = "monarchr")
#'
#' g <- file_engine(filename) |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' print(explode(g))
#' @export
explode <- function(g, ...) {
  UseMethod("explode")
}

#' @export
#' @importFrom tidygraph activate
explode.tbl_kgx <- function(g, ...) {
  nodes <- as_tibble(g, active = "nodes")
  graphs <- lapply(nodes$id, function(node_id) {
    filter(tidygraph::activate(g, nodes), id == node_id)
  })
  return(graphs)
}
