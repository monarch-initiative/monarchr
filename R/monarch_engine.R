# monarch_engine() is an instance of neo4j_engine() with the URL set to the Monarch Neo4j instance.

#' @title monarch_engine
#' @description Create a new neo4j engine object for the Monarch Neo4j instance
#' @return An object of class `neo4j_engine` with the URL set to the Monarch Neo4j instance
#' @export
#' @examples
#' g <- monarch_engine() %>%
#'  search_kg("Cystic fibrosis", limit = 1) %>%
#'  fetch_edges(predicate = "biolink:subclass_of")
#' g
monarch_engine <- function(url = "https://neo4j.monarchinitiative.org", api_url = "https://api.monarchinitiative.org/v3/api") {
  e <- neo4j_engine(url = url)
  e$preferences$monarch_api_url <- api_url
  class(e) <- c("monarch_engine", class(e))
  return(e)
}