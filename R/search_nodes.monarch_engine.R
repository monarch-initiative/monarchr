#' @export
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
#' @importFrom httr GET content http_status
search_nodes.monarch_engine <- function(engine,
                                     query,
                                     category = NULL,
                                     limit = 10,
                                     ...) {

    api_url <- paste0(engine$preferences$monarch_api_url, "/search")

    # ensure that the limit is not null and is a length-1 integer <= 500
    assert_that(!is.null(limit), is.numeric(limit), limit <= 500, msg = "limit must be a length-1 integer <= 500 for search_nodes.monarch_engine()")

    params <- list(
        "q" = query,
        "limit" = limit,
        "offset" = 0
    )

    if(!is.null(category)) {
        params$category <- category
    }

    # put the httr::GET call in a trycatch block to handle errors
    response <- GET(api_url, query = params)

    # if the response is not 200, throw an error
    if(response$status_code != 200) {
        stop(paste0("Error: ", response$status_code, " ", http_status(response$status_code)$message))
    }

    response_content <- content(response, "parsed")
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
