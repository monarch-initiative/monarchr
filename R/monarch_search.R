#' Search the Monarch Initiative
#'
#' This function uses the Monarch Initiative API to search for entities matching
#' a given query string within a specified category, returing a graph containg just the nodes in the search results.
#'
#' @param query A character string representing the query term. Example terms:
#'   "Cystic Fibrosis", "CYP6B", "swelling of joints".
#' @param category A character string indicating a single entity category in which to search for the query term.
#'   Defaults to NULL, to search in any category.
#' @param limit An integer indicating the maximum number of search results to return. Defaults to 10.
#' @details The returned graph will contain only nodes with no associations between them, even if they exist in the Monarch database.
#' @return A `tbl_kgx` graph object containing the search results as nodes, with no edges.
#'
#' @examples
#' monarch_search("Cystic Fibrosis", "biolink:Disease", 5)
#'
#' @export
monarch_search <- function(query,
                           category = NULL,
                           limit = 10) {
    api_url <- paste0(getOption("monarch_base_api_url"), "/search")

    params <- list(
        "q" = query,
        "limit" = limit,
        "offset" = 0
    )

    if(!is.null(category)) {
        params$category <- category
    }

    response <- httr::GET(api_url, query = params)
    response_content <- httr::content(response, "parsed")
    total_available <- response_content$total

    ids <- unlist(lapply(response_content$items, function(item) {
        item$id
    }))

    g <- query_graph(query = "MATCH (n) WHERE n.id IN $ids RETURN n",
                     parameters = list(ids = ids))

    return(g)
}
