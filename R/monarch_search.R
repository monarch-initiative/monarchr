# for array params in httr queries
# from https://stackoverflow.com/a/72532186
flattenbody <- function(x) {
	# A form/query can only have one value per name, so take
	# any values that contain vectors length >1 and
	# split them up
	# list(x=1:2, y="a") becomes list(x=1, x=2, y="a")
	if (all(lengths(x)<=1)) return(x);
	do.call("c", mapply(function(name, val) {
		if (length(val)==1 || any(c("form_file", "form_data") %in% class(val))) {
			x <- list(val)
			names(x) <- name
			x
		} else {
			x <- as.list(val)
			names(x) <- rep(name, length(val))
			x
		}
	}, names(x), x, USE.NAMES = FALSE, SIMPLIFY = FALSE))
}

#' Search for KG nodes using the Monarch Initiative search API
#'
#' This function is a wrapper around the Monarch-hosted
#' [search API](https://api.monarchinitiative.org/v3/docs#/search/search_v3_api_search_get).
#' It returns nodes (no edges) from the Monarch KG, fetched via an instance of `monarch_engine()`.
#'
#' @param query Search query string, e.g. "Cystic fibrosis"
#' @param category A set of node category labels to limit the search to, e.g. c("biolink:Disease", "biolink:Gene")
#' @param limit Maximum number of nodes to return. Default 10.
#' @param ... Other parameters (unused).
#' @return A local tbl_kgx graph with no edges.
#' @export
#' @import tidygraph
#' @import dplyr
#' @importFrom assertthat assert_that
#' @importFrom httr GET content http_status
#' @examplesIf monarch_engine_check()
#' cf_hits <- monarch_search("Cystic fibrosis", category = "biolink:Disease", limit = 5)
#' print(cf_hits)
#'
monarch_search <- function(query,
                           category = NULL,
                           limit = 10,
                           ...) {

		engine <- monarch_engine()
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
    response <- GET(api_url, query = flattenbody(params))

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