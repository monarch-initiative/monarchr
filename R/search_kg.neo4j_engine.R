# search function using neo4j

#' @export
search_kg.neo4j_engine <- function(e, query, category = NULL, limit = 10, ...) {
    # this function needs to generate a cypher query that uses a regex match against the properties listed
    # in the preferences in the node_search_properties from the kg_prefs.
    # it will only look at nodes that have a category entry in the supplied category vector

    # first lets build the basic regex we'll use to search
    query_regex <- paste0(".*", query, ".*")

    # let's build the query, starting with the MATCH clause
    query <- "MATCH (n)"

    # now we'll add the WHERE clause to filter by the search query
    # we need to do this for each property in the node_search_properties
    # we'll use a regex match for each property
    # we'll use the OR operator to combine the matches

    # get the node_search_properties from the kg_prefs
    prefs <- e$preferences$node_search_properties
    # those are the properties we are going to search with =~, combined with OR
    search_properties <- paste0("n.", prefs, " =~ $query_regex")
    search_properties <- paste(search_properties, collapse = " OR ")
    # wrap the search_properties in parentheses
    search_properties <- paste0("(", search_properties, ")")

    # add the search_properties to the query
    query <- paste0(query, " WHERE ", search_properties)

    # if one or more categories are supplied, we'll add a WHERE clause to filter by category
    # note that category will be an array property in the graph
    if(!is.null(category)) {
        cat_array <- paste0("'", category, "'")
        cat_array <- paste(cat_array, collapse = ",")
        query <- paste0(query, " AND ANY(category IN n.category WHERE category IN [", cat_array, "])")
    }

    # now we'll add the RETURN clause
    query <- paste0(query, " RETURN n LIMIT $limit")

    result <- cypher_query(e, query, parameters = list(query_regex = query_regex, limit = limit))
    return(result)
}
