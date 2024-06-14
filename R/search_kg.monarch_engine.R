#' Search the Monarch Initiative
#'
#' This function uses the Monarch Initiative API to search for entities matching
#' a given query string within a specified category, returing a graph containg just the nodes in the search results.
#'
#' @param engine A Monarch engine object.
#' @param query A character string representing the query term. Example terms:
#'   "Cystic Fibrosis", "CYP6B", "swelling of joints".
#' @param category A character string indicating a single entity category in which to search for the query term.
#'   Defaults to NULL, to search in any category.
#' @param limit An integer indicating the maximum number of search results to return. Defaults to 10.
#' @param ... Additional arguments (unused).
#' @details The returned graph will contain only nodes with no associations between them, even if they exist in the Monarch database.
#' @return A `tbl_kgx` graph object containing the search results as nodes, with no edges.
#'
#' @examples
#' e <- monarch_engine()
#' search(e, "Cystic Fibrosis", category = "biolink:Disease", limit = 5)
#'
#' @export
search_kg.monarch_engine <- function(engine,
                                     query,
                                     category = NULL,
                                     limit = 10,
                                     ...) {

    api_url <- paste0(engine$preferences$monarch_api_url, "/search")

    # ensure that the limit is not null and is a length-1 integer <= 500
    assert_that(!is.null(limit), is.numeric(limit), limit <= 500, msg = "limit must be a length-1 integer <= 500 for search_kg.monarch_engine()")

    params <- list(
        "q" = query,
        "limit" = limit,
        "offset" = 0
    )

    if(!is.null(category)) {
        params$category <- category
    }

    # put the httr::GET call in a trycatch block to handle errors
    response <- httr::GET(api_url, query = params)

    # if the response is not 200, throw an error
    if(response$status_code != 200) {
        stop(paste0("Error: ", response$status_code, " ", httr::http_status(response$status_code)$message))
    }

    response_content <- httr::content(response, "parsed")
    total_available <- response_content$total

    ids <- unlist(lapply(response_content$items, function(item) {
        item$id
    }))

    if(length(ids) == 0) {
        # return an empty graph
        return(tbl_kgx(nodes = data.frame(id = character(), category = list()))
        )
    }
    if(length(ids) == 1) {
        ids <- list(ids)
    }

    g <- cypher_query(engine, query = "MATCH (n) WHERE n.id IN $ids RETURN n",
                     parameters = list(ids = ids))

    return(g)
}
