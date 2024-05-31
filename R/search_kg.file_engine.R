search_kg.file_engine <- function(e, query, category = NULL, limit = 10, ...) {
    # the file engine contains a tidygraph object, so we need to get the nodes data and filter it

    nodes_df <- e$graph %>% nodes()

    # first, let's filter the nodes by the query
    query_regex <- paste0(".*", query, ".*")
    # we need to filter each column listed in the kg_prefs node_search_properties, keeping any that match the query
    prefs <- e$preferences$node_search_properties

    match_bools <- rep(FALSE, nrow(nodes_df))
    # these are the rows that match the query in any of the searchable cols
    for(p in prefs) {
        if(p %in% colnames(nodes_df)) {
            # ensure that col p is a character vector or factor; if not, skip it and warn the user
            if(!is.character(nodes_df[[p]]) & !is.factor(nodes_df[[p]])) {
                warning(paste0("Column ", p, " is not a character vector or factor; skipping."))
                next
            }
            match_bools <- match_bools | grepl(query_regex, nodes_df[[p]])
        }
    }

    # now, we need to identify which rows the category (which is a list column) contains any of the specified category values (which is a character vector)
    # the length should be the same as match_bools
    if(!is.null(category)) {
        # this isn't working...
        match_cats <- nodes_df$category %>% lapply(function(x) any(x %in% category)) %>% unlist()
    }

    sub_nodes_df <- head(nodes_df[match_bools & match_cats, ], limit)

    ret_graph <- tbl_kgx(nodes = sub_nodes_df)

    return(ret_graph)
}