base_engine <- function(name = "default_engine", preferences = NULL, ...) {
    if(!is.null(preferences)) {
        if(file.exists(preferences)) {
            preferences <- yaml::read_yaml(preferences)
        }
    }

    obj <- list(name = name,
                preferences = preferences)

    class(obj) <- c("base_engine", class(obj))
    return(obj)
}

# constructor for neo4j_engine
neo4j_engine <- function(url = "https://neo4j.monarchinitiative.org",
                         username = NA,
                         password = NA,
                         preferences = NULL,
                         ...) {


	graph_conn <- neo2R::startGraph(url, username = username, password = password, ...)

    obj <- base_engine(name = "neo4j_engine",
                       preferences = preferences)

    obj$graph_conn <- graph_conn
    obj$url <- url
    obj$username <- username

    class(obj) <- c("neo4j_engine", class(obj))
    return(obj)
}

# generic function for cypher_query_df
cypher_query_df <- function(engine, ...) {
    UseMethod("cypher_query_df")
}

# generic method for cypher_query
cypher_query <- function(engine, ...) {
    UseMethod("cypher_query")
}

library(dplyr)
library(assertthat)
library(tidygraph)
e <- neo4j_engine()
# example must return table
res <- cypher_query(e, "MATCH (n) RETURN n LIMIT 10")


# generic method for field_search
field_search <- function(engine, ...) {
    UseMethod("field_search")
}

field_search.neo4j_engine <- function(engine, field, value, ...) {
    query <- paste0("MATCH (n) WHERE n.", field, " = $value RETURN n")
    parameters <- list(value = value)
    res <- cypher_query(engine, query, parameters)
    return(res)
}

# generic method for fetch_edges
fetch_edges <- function(engine, ...) {
    UseMethod("fetch_edges")
}



g <- e %>% field_search("id", "MONDO:0007525")

g2 <- e %>% fetch_edges(g, direction = "out")

g3 <- g %>% fetch_edges(direction = "out")

