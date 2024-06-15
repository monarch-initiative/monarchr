#' Search for nodes in a knowledge graph
#'
#' Searches for nodes in a knowledge graph using a given query string, optionally restricting the search to specific node categories (e.g. biolink:Disease).
#'
#' The search functionality is specific to the engine hosting the knowledge graph (e.g. `file_engine()`, `neo4j_engine()`, `monarch_engine()`). For 
#' both the `file_engine()` and `neo4j_engine()`, the search is performed using regular expressions on specific node properties, defaulting to
#' `id`, `name`, and `description`. A search for `"Cystic fibrosis"`` is expanded to `".*Cystic fibrosis.*"``, allowing the use of regular expressions
#' such as `"[Cc]ystic fibrosis"`.
#'
#' The `monarch_engine()` uses the Monarch Initiative API to search for entities matching a given query string within a specified category, and supports a maximum of 500 search results.
#' To use regular-expression based search with the hosted Monarch Iniative database, instantiate a `neo4j_engine()` with the appropriate URL (`https://neo4j.monarchinitiative.org`).
#'
#' @param engine A graph engine object
#' @param query A length-1 character string representing the query term.
#' @param category A character vector indicating the entity categories in which to search for the query term. Defaults to `NULL`, to search in any category.
#' @param limit An integer indicating the maximum number of search results to return. Defaults to 10.
#' @param ... Additional arguments (unused).
#' @return a tbl_kgx graph
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidygraph)
#'
#' @examplesIf monarch_engine_check()
#' # the monarch engine uses the Monarch initiative search api
#' monarch_engine() |>
#'   search_nodes("cystic fibrosis",
#'             category = "biolink:Disease",
#'             limit = 5) |>
#'   activate(nodes) |>
#'   as.data.frame() |>
#'   select(id, name, pcategory)
#'
#' @examplesIf neo4j_engine_check("https://neo4j.monarchinitiative.org")
#' # the neo4j engine searches a given property for regex matches
#' neo4j_engine(url = "https://neo4j.monarchinitiative.org") |>
#'   search_nodes("MONDO:*",
#'             limit = 5) |>
#'   activate(nodes) |>
#'   as.data.frame() |>
#'   select(id, name, pcategory)
#'
#'
#' # we can change the search properties for the neo4j engine,
#' # for example to search name only
#' neo4j_engine(url = "https://neo4j.monarchinitiative.org",
#'              preferences = list(node_search_properties = c("name"))) |>
#'   search_nodes("cystic fibrosis",
#'             category = "biolink:Disease",
#'             limit = 5) |>
#'   activate(nodes) |>
#'   as.data.frame() |>
#'   select(id, name, pcategory)
#'
#' @examples
#' # file_engine supports the same features as neo4j_engine
#' # (using the MONDO KGX file packaged with monarchr)
#' filename <- system.file("extdata", "mondo_kgx_tsv.tar.gz", package = "monarchr")
#' file_engine(filename) |>
#'   search_nodes("cystic fibrosis", 
#'             category = "biolink:Disease",
#'             limit = 5) |>
#'   activate(nodes) |>
#'   as.data.frame() |>
#'   select(id, name, pcategory)
#'
search_nodes <- function(engine, query, category = NULL, limit = 10, ...) {
    UseMethod("search_nodes")
}
