#' Search the Monarch Initiative
#'
#' This function uses the Monarch Initiative API to search for entities matching
#' a given query string within a specified category.
#'
#' @param query A character string representing the query term. Example terms:
#'   "Cystic Fibrosis", "CYP6B", "swelling of joints".
#' @param category A character string indicating the entity category in which to search for the query term.
#'   Defaults to "biolink:Disease".
#' @param limit An integer indicating the maximum number of search results to return. Defaults to 10.
#' @param offset An integer indicating the number of search results to skip (for pagination). Defaults to 0.
#' @details The returned graph will contain only nodes with no associations between them, even if they exist in the Monarch database.
#' @return A `monarch_kg` graph object containing the search results as nodes, with no edges.
#'
#' @examples
#' monarch_search("Cystic Fibrosis", "biolink:Disease", 5)
#'
#' @export
# monarch_search <- function(query, category = "biolink:Disease", limit = 10, offset = 0) {
#     api_url <- paste0(getOption("monarchr.base_api_url"), "/search")

#     params <- list(
#         "q" = query,
#         "category" = category,
#         "limit" = limit,
#         "offset" = offset
#     )

#     response <- httr::GET(api_url, query = params)
#     response_content <- httr::content(response, "parsed")

#     seeds <- lapply(response_content$items, function(item) {
#         seed_graph(item$id)
#     })

#     g <- seed_graph() # get an empty graph with the right columns
#     for(seed in seeds) {
#         suppressMessages(g <- tidygraph::graph_join(g, seed))
#     }

#     return(as.monarch_kg(g))
# }

monarch_search <- function(query,
                           category = "biolink:Disease",
                           limit = 10,
                           results_per_request = 25) {
    api_url <- paste0(getOption("monarchr.base_api_url"), "/search")

    pb <- progress::progress_bar$new(total = limit)

    total_collected <- 0
    g <- seed_graph() # get an empty graph with the right columns

    total_available <- 0

    while (total_collected < limit) {
        params <- list(
            "q" = query,
            "category" = category,
            "limit" = min(results_per_request, limit - total_collected), # Request the minimum of results_per_request or remaining number of items to reach the limit
            "offset" = total_collected
        )

        response <- httr::GET(api_url, query = params)
        response_content <- httr::content(response, "parsed")
        total_available <- response_content$total

        seeds <- lapply(response_content$items, function(item) {
            seed_graph(item$id)
        })

        for(seed in seeds) {
            suppressMessages(g <- tidygraph::graph_join(g, seed))
            pb$tick()
        }

        total_collected <- total_collected + length(response_content$items)

        # If we've collected enough items, or if there are no more items available, stop
        if (total_collected >= limit || total_collected >= response_content$total) {
            break
        }
    }

    if(limit < total_available) {
        message(paste0("\n\nNOTE: Only ", limit, " of ", total_available, " results returned.\n"))
    }

    return(as.monarch_kg(g))
}
