######### Non-Exported Utility Functions #########

#' This function takes an R expression and converts it to Cypher syntax.
#' 
#' @param expr An R expression
#' @return A string containing the expression in Cypher syntax
#' @examples
#' expr_to_cypher(~ id == "MONDO:0007525")
#' expr_to_cypher(~ id == "MONDO:0007525" & category == "biolink:Disease")
expr_to_cypher <- function(expr) {
  # Convert expression to text, preserving structure
  expr <- rlang::expr_text(expr)
  
  # Remove the tilde (~) if present
  expr <- gsub("^~\\s*", "", expr)

  # Replace R operators with Cypher operators
  expr <- stringr::str_replace_all(expr, stringr::fixed("%in%"), "IN")
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



#' This function generates a Cypher query based on a set of conditions.
#' 
#' @param ... A set of conditions
#' @param limit The maximum number of results to return
#' @details This function takes a set of conditions and generates a Cypher query that 
#' can be used to query the Monarch knowledge graph. The conditions should be R expressions,
#' and can be combined using the logical operators `&` (AND) and `|` (OR). The `==` operator
#' is used to test for equality, and the `%in%` operator is used to test for membership in a list.
#' Also supported are the `!` (NOT) operator, `!=`, and paretheses for grouping. 
#' Uses tidy syntax, and multiple conditions supplied as parameters are combined with `AND`.
#' 
#' Note that expression like fetch_nodes(id %in% c("MONDO:0007525", "HGNC:4635")) will *not* work; see query_ids().
#' @return A string containing the Cypher query
#' @examples
#' generate_cypher_query(id == "MONDO:0007525")
#' generate_cypher_query(id == "MONDO:0007525" & category == "biolink:Disease")
#' generate_cypher_query("biolink:Disease" %in% category | "biolink:Gene" %in% category)
generate_cypher_query <- function(..., limit = 10, skip = 0) {
  # Capture the expressions
  conditions <- rlang::enquos(...)
  

  # Translate the conditions to Cypher syntax
  condition_strings <- purrr::map_chr(conditions, expr_to_cypher)

  # Build the Cypher query
  query <- paste(
    "MATCH (n)",
    if (length(condition_strings) > 0) paste("WHERE", paste(condition_strings, collapse = " AND ")) else "",
    "RETURN n",
    "SKIP", skip,
    "LIMIT", limit
  )

  # Return the complete Cypher query
  return(query)
}

######### Exported Functions #########

#' Fetch nodes from a graph
#' 
#' This function fetches nodes from a graph based on a set of conditions.
#' 
#' @param engine A graph engine object
#' @param id A character vector of identifiers to fetch
#' @param ... A set of conditions identifying the nodes to fetch, only used if id is NULL
#' @return A tbl_kgx object containing the nodes
#' @examples
#' e <- neo4j_engine()
#' g <- fetch_nodes(e, id == c()"MONDO:0007525", "MONDO:0007526"))
#' g <- fetch_nodes(e, "biolink:Disease" %in% category | "biolink:Gene" %in% category)
#' g <- fetch_nodes(e, "biolink:Disease" %in% category, limit = 5)
#' @export
fetch_nodes.neo4j_engine <- function(engine, id = NULL, ...) {
    if(!is.null(id)) {
        res <- cypher_query(engine,
                            query = "MATCH (n) WHERE n.id IN $id RETURN n",
                            parameters = list(id = id))
    } else {
        query <- generate_cypher_query(...)
        res <- cypher_query.neo4j_engine(engine, query)
    }

    return(res)
}
