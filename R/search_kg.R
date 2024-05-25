# generic function for search_kg

#' search_kg
#'
#' Search a knowledge graph using a specific engine. Different engines
#' support different search capabilities and parameters.
#'
#' @param obj a graph engine object
#' @param ... additional parameters
#' @return a tbl_kgx graph
#' @export
#'
#' @examples
#' # the monarch engine uses the Monarch initiative search api
#' e <- monarch_engine()
#' g <- search_kg(e, "cystic fibrosis", category = "biolink:Disease", limit = 20)
#' 
#' # the neo4j engine searches a given property for regex matches
#' e <- neo4j_engine(url = "https://neo4j.monarchinitiative.org")
#' g <- search_kg(e, "MONDO:.*", field = "id", limit = 20)
#' 
search_kg <- function(obj, ...) {
    UseMethod("search_kg")
}
