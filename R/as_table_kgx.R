# Define the generic function
as_table_kgx <- function(x, ...) {
    UseMethod("as_table_kgx")
}

# Define the specific method for character vector inputs
as_table_kgx.character <- function(x, ...) {
    query_graph(x, ...)
}
