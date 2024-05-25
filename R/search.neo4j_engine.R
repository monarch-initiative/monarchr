# search function using neo4j

search.neo4j_engine <- function(e, query_regex, field = "id", limit = 10, ...) {
    # returns a tbl_kgx graph
    # query is a string
    # field is a string
    # limit is an integer
    # e is a graph engine object
    # returns a tbl_kgx graph
    query <- paste0("MATCH (n) WHERE n.", field, " =~ $query_regex RETURN n LIMIT $limit")
    result <- cypher_query(e, query, parameters = list(query_regex = query_regex, limit = limit))
    return(result)
}