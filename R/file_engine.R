## Work in progress

#
# filename <- "https://kghub.io/kg-obo/vbo/2023-06-02/vbo_kgx_tsv.tar.gz"

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