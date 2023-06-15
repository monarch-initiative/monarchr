
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
monarch_search <- function(query, category = "biolink:Disease", limit = 10, offset = 0) {
	# For a given query term, find entities within a given category, e.g. biolink:Disease, biolink:Gene, biolink:Phenotype
	# example query terms: "Cystic Fibrosis", "CYP6B", "swelling of joints"
	# TODO: handle pagination in this function
	api_url <- paste0(getOption("monarchr.base_api_url"), "/search")

	params <- list(
		"q" = query,
		"category" = category,
		"limit" = limit,
		"offset" = offset
	)

	response <- httr::GET(api_url, query = params)
	response_content <- httr::content(response, "parsed")

	seeds <- lapply(response_content$items, function(item) {
		seed_graph(item$id)
	})

	g <- seed_graph() # get an empty graph with the right columns
	for(seed in seeds) {
		suppressMessages(g <- tidygraph::graph_join(g, seed))
	}

	return(minikg(g))
}
