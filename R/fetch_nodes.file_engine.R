#' @export
#' @import tidygraph
#' @import dplyr
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

    # we want to drop all the edges to be compatible with the neo4j engine
    res <- res %>%
        activate(edges) %>%
        filter(FALSE) %>%
        activate(nodes)

    attr(res, "last_engine") <- engine

    return(res)
}