#' Create a knowledge graph engine object from a KGX-based tsv file
#'
#' Creates a knowledge graph engine backed by a KGX-based tab-separated file. This must be a filename or URL to a `.tar.gz` file containing a `*_nodes.tsv` and `*_edges.tsv` file. If a URL is provided, the file will be downloaded to the user's current working directory.
#'
#' Engines store preference information specifying how data are fetched and manipulated; for example,
#' while node `category` is multi-valued (nodes may have multiple categories, for example "biolink:Gene" and "biolink:NamedThing"),
#' typically a single category is used to represent the node in a graph, and is returned as the nodes' `pcategory`. A preference list of categories to use for `pcategory` is
#' stored in the engine's preferences. A default set of preferences is stored in the package for use with KGX (BioLink-compatible) graphs (see https://github.com/biolink/kgx/blob/master/specification/kgx-format.md),
#' but these can be overridden by the user.
#'
#' @param filename A character string indicating the filename or URL of the KGX-based tsv file.
#' @param preferences A named list of preferences for the engine.
#' @param ... Additional arguments (unused).
#' @seealso `neo4j_engine()`, `monarch_engine()`
#' @return An object of class `file_engine`
#' @export
#' @examples
#' library(tidygraph)
#' library(dplyr)
#'
#' # Using example KGX .tar.gz file packaged with monarchr
#' filename <- system.file("extdata", "eds_marfan_kg.tar.gz", package = "monarchr")
#' engine <- file_engine(filename)
#'
#' res <- engine |> fetch_nodes(query_ids = c("MONDO:0007522", "MONDO:0007947"))
#' print(res)
#'
#' @importFrom archive archive_read
#' @importFrom readr col_character
#' @importFrom readr read_tsv
#' @importFrom readr cols
#' @importFrom utils download.file
#' @importFrom utils untar
file_engine <- function(filename, preferences = NULL, ...) {
    obj <- base_engine(name = "file_engine", preferences = preferences)
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

    # suppresses non-fatal warnings; it appears that many kgs in kghub
    # have more column headers than columns which causes vroom to issue a warning
    nodes <- suppressWarnings(classes = "vroom_parse_issue",
                              readr::read_tsv(archive::archive_read(filename, file = nodes_file),
                              col_types = readr::cols(id = readr::col_character(), category = readr::col_character()),
                              show_col_types = FALSE)
                             )

    edges <- suppressWarnings(classes = "vroom_parse_issue",
                              readr::read_tsv(archive::archive_read(filename, file = edges_file),
                              col_types = readr::cols(subject = readr::col_character(), predicate = readr::col_character(), object = readr::col_character()),
                              show_col_types = FALSE)
                             )

    content_check <- check_kgx_file_ok(nodes, edges)
    if(!content_check[[1]]) {
    	message <- paste0(c("There appears to be an issue with the graph content or formatting. The following errors are noted:",
    											content_check[[2]]), collapse = "\n   ")
    	stop(message)
    }

    # although we read category in as a character vector, it should be a list column
    # if there are multiple categories for a node, they will be separated with |
    # characters per the KGX spec: https://github.com/biolink/kgx/blob/master/specification/kgx-format.md#core-node-record-elements
    nodes$category <- strsplit(nodes$category, "\\|")

    # let's also look for any other columns that are list columns, by seeing if they contain | characters
    # we'll split these columns into list columns
    list_cols <- names(nodes)[sapply(nodes, function(x) any(grepl("\\|", x)))]
    # drop the 'description', 'name', and 'id' columns though, those should never be lists
    list_cols <- list_cols[!list_cols %in% c("description", "name", "id")]

    for(col in list_cols) {
        nodes[[col]] <- strsplit(nodes[[col]], "\\|")
    }

    nodes$pcategory <- normalize_categories(nodes$category, obj$preferences$category_priority)

    obj$graph <- tbl_kgx(nodes = nodes, edges = edges)

    class(obj) <- c("file_engine", class(obj))
    return(obj)
}

#' @noRd
check_kgx_file_ok <- function(nodes_df, edges_df) {
	node_check <- check_node_kgx_df(nodes_df)
	edge_check <- check_edge_kgx_df(edges_df)
	ok <- all(node_check[[1]] & edge_check[[1]])
	errors <- c(node_check[[2]], edge_check[[2]])
	return(list(ok, errors))
}

#' @noRd
check_node_kgx_df <- function(nodes_df) {
	ok <- TRUE
	errors <- c()
	if(!"id" %in% names(nodes_df)) {
		ok <- FALSE
		errors <- c(errors, "No `id` column found in the nodes file. Is the archive a proper KGX-formatted knowledge graph? Nodes in KGX formatted graphs must have a character `id` column formatted as a CURIE (with entries e.g. 'MONDO:0019391').")
	}
	if(!"category" %in% names(nodes_df)) {
		ok <- FALSE
		errors <- c(errors, "No `category` column found in the nodes file. Is the archive a proper KGX-formatted knowledge graph? Nodes in KGX formatted graphs must have a multi-valued, |-separated `category` column (with entries e.g. 'biolink:Entity|biolink:NamedThing|biolink:Cell').")
	}
	return(list(ok, errors))
}

#' @noRd
check_edge_kgx_df <- function(edges_df) {
	ok <- TRUE
	errors <- c()
	if(!"subject" %in% names(edges_df)) {
		ok <- FALSE
		errors <- c(errors, "No `subject` column found in the edges file. Is the archive a proper KGX-formatted knowledge graph? Edges in KGX formatted graphs must have a `subject` column (with entries e.g. 'MONDO:0019391').")
	}
	if(!"predicate" %in% names(edges_df)) {
		ok <- FALSE
		errors <- c(errors, "No `predicate` column found in the edges file. Is the archive a proper KGX-formatted knowledge graph? Edges in KGX formatted graphs must have a `predicate` column (with entries e.g. 'biolink:has_phenotype').")
	}
	if(!"object" %in% names(edges_df)) {
		ok <- FALSE
		errors <- c(errors, "No `object` column found in the edges file. Is the archive a proper KGX-formatted knowledge graph? Edges in KGX formatted graphs must have an `object` column (with entries e.g. 'HP:0004322').")
	}
	return(list(ok, errors))
}



