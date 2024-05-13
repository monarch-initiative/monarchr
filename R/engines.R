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



filename <- "https://kghub.io/kg-obo/vbo/2023-06-02/vbo_kgx_tsv.tar.gz"

file_engine <- function(filename, preferences = NULL, ...) {
    obj <- base_engine(name = "file_engine")
    obj$filename <- filename

    # a .tar.gz file should have a *_nodes.tsv and *_edges.tsv file
    # we should check for these files and load them into the engine

    # if the file is a URL, download it
    # save it in the users' current working directory

    if(grepl("^http", filename)) {
        base <- basename(filename)
        download.file(filename, destfile = base)
        filename <- base
    }


    if(!file.exists(filename)) {
        stop("File does not exist.")
    }

    # ensure that files is a .tar.gz
    if(!grepl(".tar.gz$", filename)) {
        stop("File must be a .tar.gz file.")
    }
   
    files <- untar(filename, list = TRUE)

    nodes_file <- grep("_nodes.tsv$", files, value = TRUE)
    edges_file <- grep("_edges.tsv$", files, value = TRUE)

    if(length(nodes_file) == 0) {
        stop("No nodes file found.")
    }

    if(length(edges_file) == 0) {
        stop("No edges file found.")
    }

    nodes <- readr::read_tsv(archive::archive_read(filename, file = nodes_file))
    edges <- readr::read_tsv(archive::archive_read(filename, file = edges_file))

    str(nodes)
    str(edges)

    ofj$graph <- tbl_kgx(nodes = nodes, edges = edges)

    class(obj) <- c("file_engine", class(obj))
    return(obj)
}

fe <- file_engine(filename)
