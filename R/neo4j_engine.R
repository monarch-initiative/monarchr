
#' Create a knowledge graph engine object for a neo4j database
#'
#' Creates a knowledge graph engine backed by a neo4j database, from a URL and optional username and password. Knowledge graph "engines"
#' are objects that store information about how to connect to a (potentially large) knowledge graph, and can be used to fetch nodes and edges from the database as local
#' graph objects.
#'
#' Engines store preference information specifying how data are fetched and manipulated; for example,
#' while node `category` is multi-valued (nodes may have multiple categories, for example "biolink:Gene" and "biolink:NamedThing"),
#' typically a single category is used to represent the node in a graph, and is returned as the nodes' `pcategory`. A preference list of categories to use for `pcategory` is
#' stored in the engine's preferences. A default set of preferences is stored in the package for use with KGX (BioLink-compatible) graphs (see https://github.com/biolink/kgx/blob/master/specification/kgx-format.md),
#' but these can be overridden by the user.
#'
#' For `neo4j_engine()`s, preferences are also used to set the node properties to search when using `search_nodes()`, defaulting to regex-based searches on id, name, and description. (The `monarch_engine()` is a type
#' of `neo4j_engine()` with the URL set to the Monarch Neo4j instance, and overrides `search_nodes()` to use the Monarch search API, see `monarch_engine()` for details).
#'
#' @param url A character string indicating the URL of the neo4j database.
#' @param username A character string indicating the username for the neo4j database (if needed).
#' @param password A character string indicating the password for the neo4j database (if needed).
#' @param preferences A named list of preferences for the engine.
#' @param ... Additional arguments passed to `neo2R::startGraph()`.
#' @seealso `file_engine()`, `monarch_engine()`
#' @return An object of class `neo4j_engine`
#' @export
#' @examplesIf neo4j_engine_check("https://neo4j.monarchinitiative.org")
#' library(tidygraph)
#' library(dplyr)
#'
#' engine <- neo4j_engine(url = "https://neo4j.monarchinitiative.org")
#' res <- engine |> fetch_nodes(query_ids = c("MONDO:0007522", "MONDO:0007947"))
#' print(res)
#'
neo4j_engine <- function(url,
                         username = NA,
                         password = NA,
                         preferences = NULL,
                         ...) {

	graph_conn <- neo2R::startGraph(url, username = username, password = password, ...)

    obj <- base_engine(name = "neo4j_engine",
                       preferences = preferences)

    obj$graph_conn <- graph_conn
    obj$url <- url
    obj$username <- username

    class(obj) <- c("neo4j_engine", class(obj))
    return(obj)
}
