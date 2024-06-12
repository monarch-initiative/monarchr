## Work in progress

#
# filename <- "https://kghub.io/kg-obo/vbo/2023-06-02/vbo_kgx_tsv.tar.gz"

#' File-based knowledge graph engine
#'
#' Returns a Knowledge Graph engine backed by a file hosted by kghub.io. Must be a `_tsv.tar.gz` file containing nodes and edges tab-separated files in KGX format. See https://kghub.io for details.
#'
#' @param filename The filename or URL to use.
#' @param preferences Override default preferences.
#' @param ... Other parameter (unused).
#'
#' @return
#' @export
#'
#' @examples
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
