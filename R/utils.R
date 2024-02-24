#' @importFrom tidygraph as_tibble
#' @examples
#' g <- monarch_search("fanconi anemia", limit = 1)
#' print(nodes(g))
nodes.tbl_kgx <- function(x, ...) {
  tidygraph::as_tibble(x, active = "nodes")
}

#' @importFrom tidygraph as_tibble
#' @examples
#' g <- monarch_search("fanconi anemia", limit = 1)
#' print(edges(g))
edges.tbl_kgx <- function(x, ...) {
  tidygraph::as_tibble(x, active = "edges")
}


#' Get graph nodes table.
#'
#' @param x A graph object
#' @return A tibble with the nodes of the graph
#' @importFrom tidygraph as_tibble
#' @export
#' @examples
#' g <- monarch_search("fanconi anemia", limit = 1)
#' print(nodes(g))
#' print(edges(g))
nodes <- function(x, ...) {
  UseMethod("nodes")
}

#' @importFrom tidygraph as_tibble
#' @export
#' @examples
#' g <- monarch_search("fanconi anemia", limit = 1)
#' print(nodes(g))
#' print(edges(g))
edges <- function(x, ...) {
  UseMethod("edges")
}


explode <- function(x, ...) {
  UseMethod("explode")
}

#' Explode a graph into a list of single-node graphs.
#'
#' @param x A graph object
#' @return A list of single-node graphs
#' @importFrom tidygraph as_tibble
#' @export
#' @examples
#' g <- monarch_search("fanconi anemia", limit = 5)
#' print(explode(g))
explode.tbl_kgx <- function(x, ...) {
  nodes <- as_tibble(x, active = "nodes")
  graphs <- lapply(nodes$id, function(node_id) {
    filter(tidygraph::activate(x, nodes), id == node_id)
  })
  return(graphs)
}
