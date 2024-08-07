#' @export
#' @importFrom tidygraph as_tibble
nodes.tbl_kgx <- function(graph, ...) {
  tidygraph::as_tibble(graph, active = "nodes")
}


#' @export
#' @importFrom tidygraph as_tibble
edges.tbl_kgx <- function(graph, ...) {
  tidygraph::as_tibble(graph, active = "edges")
}


#' Get graph nodes table.
#'
#' @param graph A graph object
#' @param ... Other options (unused)
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
nodes <- function(graph, ...) {
  UseMethod("nodes")
}

#' Get graph edges table.
#'
#' @param graph Input graph
#' @param ... Other options (unused)
#' @export
#' @return A tibble with the edges of the graph
#' @importFrom tidygraph as_tibble
#' @examples
#' # (using the MONDO KGX file packaged with monarchr)
#' filename <- system.file("extdata", "mondo_kgx_tsv.tar.gz", package = "monarchr")
#'
#' g <- file_engine(filename) |>
#'   fetch_nodes(query_ids = c("MONDO:0007525", "MONDO:0007526"))
#'
#' print(edges(g))
edges <- function(graph, ...) {
  UseMethod("edges")
}

#' Explode a graph into a list of single-node graphs
#'
#' @param graph A tbl_kgx graph.
#' @param ... Other options (unused)
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
explode <- function(graph, ...) {
  UseMethod("explode")
}

#' @export
#' @importFrom tidygraph activate
explode.tbl_kgx <- function(graph, ...) {
  nodes <- as_tibble(graph, active = "nodes")
  graphs <- lapply(nodes$id, function(node_id) {
    filter(tidygraph::activate(graph, nodes), id == node_id)
  })
  return(graphs)
}
