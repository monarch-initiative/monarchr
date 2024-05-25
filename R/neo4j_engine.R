
#' @title neo4j_engine
#' @description Create a new neo4j engine object
#' @param url A character string indicating the URL of the neo4j database.
#' @param username A character string indicating the username for the neo4j database (if needed).
#' @param password A character string indicating the password for the neo4j database (if needed).
#' @param preferences A named list of preferences for the engine.
#' @param ... Additional arguments passed to the function.
#' @return An object of class `neo4j_engine`
#' @export
#' @examples
#' neo4j_engine(url = "https://neo4j.monarchinitiative.org")
#' neo4j_engine(url = "https://neo4j.monarchinitiative.org", username = "neo4j", password = "password")
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
