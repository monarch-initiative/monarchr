
#' Fetch nodes from a graph
#' 
#' This function fetches nodes from a graph based on a set of conditions.
#' 
#' @param engine A graph engine object
#' @param ... A set of conditions identifying the nodes to fetch, only used if query_ids is NULL
#' @param query_ids A character vector of identifiers to fetch
#' @return A tbl_kgx object containing the nodes
#' @examples
#' e <- file_engine(system.file("extdata", "mondo_kgx_tsv.tar.gz", package = "monarchr"))
#' g <- fetch_nodes(e, id == c("MONDO:0007525", "MONDO:0007526"))
#' g <- fetch_nodes(e, "biolink:Disease" %in% category | "biolink:Gene" %in% category)
#' g <- fetch_nodes(e, "biolink:Disease" %in% category, limit = 5)
#' @export
fetch_nodes.file_engine <- function(engine, ..., query_ids = NULL) {
    if(!is.null(query_ids)) {
        res <- engine$graph %>% 
            activate(nodes) %>% 
            filter(id %in% query_ids)
    } else {
        res <- engine$graph %>% 
            activate(nodes) %>% 
            filter(...)
    }

    return(res)
}