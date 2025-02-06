#' Create a knowledge graph engine object backed by the public Monarch Neo4j instance
#'
#' Creates a knowledge graph engine backed by the publicly hosted Monarch Neo4j database, used to fetch nodes and edges from the database as local
#' graph objects.
#'
#' Engines store preference information specifying how data are fetched and manipulated; for example,
#' while node `category` is multi-valued (nodes may have multiple categories, for example "biolink:Gene" and "biolink:NamedThing"),
#' typically a single category is used to represent the node in a graph, and is returned as the nodes' `pcategory`. A preference list of categories to use for `pcategory` is
#' stored in the engine's preferences. A default set of preferences is stored in the package for use with this and other KGX (BioLink-compatible) graphs (see https://github.com/biolink/kgx/blob/master/specification/kgx-format.md),
#' but these can be overridden by the user.
#'
#' The `monarch_engine()` overrides `search_nodes()` to use the Monarch search API, so setting `node_search_properties` in the preferences will not affect the search behavior. To use regex-based searching with the Monarch Neo4j instance, use `neo4j_engine()` instead and specify the Monarch Neo4j URL (https://neo4j.monarchinitiative.org).
#'
#' @param url (Optional) May be specified to override the default Monarch Neo4j URL. If given a vector, each will be tried in sequence; if a URL times out (see timeout) or fails, the next is tried.
#' @param api_url (Optional) May be specified to override the default Monarch API URL (specifying the location of the `/search` endpoint used by `search_nodes()`).
#' @param preferences A named list of preferences for the engine.
#' @param timeout Number of seconds to wait before trying the next url.
#' @param ... Additional arguments passed to `neo2R::startGraph()`.
#' @seealso `file_engine()`, `neo4j_engine()`
#' @return An object of class `monarch_engine`
#' @export
#' @examplesIf monarch_engine_check()
#' library(tidygraph)
#' library(dplyr)
#'
#' monarch <- monarch_engine()
#' res <- monarch |> fetch_nodes(query_ids = c("MONDO:0007522", "MONDO:0007947"))
#' print(res)
#'
monarch_engine <- function(url = c("https://neo4j.monarchinitiative.org",
																	 "http://neo4j.monarchinitiative.org",
																	 "https://neo4j.monarchinitiative.org:7473",
																	 "http://neo4j.monarchinitiative.org:7473"),
                           api_url = "https://api.monarchinitiative.org/v3/api",
                           preferences = NULL,
													 timeout = 2,
                           ...) {
  e <- neo4j_engine(url = url, preferences = preferences, timeout = timeout, ...)
  e$preferences$monarch_api_url <- api_url
  class(e) <- c("monarch_engine", class(e))
  return(e)
}
