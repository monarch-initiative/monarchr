
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
#' @param url A character string indicating the URL of the neo4j database. If given a vector, each will be tried in sequence; if a URL times out (see timeout) or fails, the next is tried.
#' @param username A character string indicating the username for the neo4j database (if needed).
#' @param password A character string indicating the password for the neo4j database (if needed).
#' @param preferences A named list of preferences for the engine.
#' @param timeout Number of sections to wait before trying the next url.
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
#' @importFrom neo2R startGraph
#' @importFrom R.utils withTimeout
neo4j_engine <- function(url,
                         username = NA,
                         password = NA,
                         preferences = NULL,
												 timeout = 1,
                         ...) {

    success <- FALSE
    for (i in seq_along(url)) {
        message(paste0("Trying to connect to ", url[i]))
        tryCatch({
            graph_conn <- withTimeout(startGraph(url[i],
                                                 username = username,
                                                 password = password,
                                                 ...),
                                      timeout = timeout,
                                      onTimeout = "error")
            success <- TRUE
        }, error = function(e) {
            message(paste0("Failed to connect to ", url[i], ": ", e$message))
        })
        if (success) {
            message(paste0("Connected to ", url[i]))
            break
        }
    }

    if (!success) {
        stop("Failed to connect to any of the URLs provided.")
    }

    obj <- base_engine(name = "neo4j_engine",
                       preferences = preferences)

    obj$graph_conn <- graph_conn
    obj$url <- url
    obj$username <- username

    class(obj) <- c("neo4j_engine", class(obj))
    return(obj)
}
