######### Non-Exported Utility Functions #########

#' @importFrom stringr str_replace_all
expr_to_cypher <- function(expr) {
  # Convert expression to text, preserving structure
  expr <- rlang::expr_text(expr)

  # Remove the tilde (~) if present
  expr <- gsub("^~\\s*", "", expr)

  # Replace R operators with Cypher operators
  expr <- stringr::str_replace_all(expr, stringr::fixed("%in_list%"), "IN")
  expr <- stringr::str_replace_all(expr, stringr::fixed("=="), "=")
  expr <- stringr::str_replace_all(expr, stringr::fixed("!="), "<>")
  expr <- stringr::str_replace_all(expr, stringr::fixed("=="), "=")
  expr <- stringr::str_replace_all(expr, stringr::fixed("&"), "AND")
  expr <- stringr::str_replace_all(expr, stringr::fixed("|"), "OR")
  expr <- stringr::str_replace_all(expr, stringr::fixed("!"), "NOT")

  # Properly handle property referencing for equality
  expr <- stringr::str_replace_all(expr, stringr::regex("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\b(?=\\s*=)"), "n.\\1")

  # Handle IN where the property name should be on the right side
  expr <- stringr::str_replace_all(expr, stringr::regex("(IN\\s+)(\\b[a-zA-Z_][a-zA-Z0-9_]*\\b)"), "\\1n.\\2")

  # Handle string literals correctly
  expr <- stringr::str_replace_all(expr, "\"", "'")

  return(expr)
}


#' @importFrom rlang enquos
#' @importFrom purrr map_chr
generate_cypher_query <- function(...) {
  # Capture the expressions
  conditions <- rlang::enquos(...)

  # Translate the conditions to Cypher syntax
  condition_strings <- purrr::map_chr(conditions, expr_to_cypher)

  # Build the Cypher query
  query <- paste(
    "MATCH (n)",
    if (length(condition_strings) > 0) paste("WHERE", paste(condition_strings, collapse = " AND ")) else "",
    "RETURN n"
  )

  # Return the complete Cypher query
  return(query)
}

######### Exported Functions #########

#' @export
#' @import tidygraph
#' @import dplyr
#' @import stringr
get_nodes.neo4j_engine <- function(engine, ..., query_ids = NULL) {
    if(!is.null(query_ids)) {
        # if query_ids is of length 1, we need to wrap it in a list for it to be sent as an array param
        if(length(query_ids) == 1) {
            query_ids <- list(query_ids)
        }
        res <- cypher_query(engine,
                            query = "MATCH (n) WHERE n.id IN $id RETURN n",
                            parameters = list(id = query_ids))
    } else {
        query <- generate_cypher_query(...)
        res <- cypher_query.neo4j_engine(engine, query)
    }

    return(res)
}
