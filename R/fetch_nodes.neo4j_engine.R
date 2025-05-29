######### Non-Exported Utility Functions #########

#' @importFrom stringr str_replace_all
expr_to_cypher <- function(expr) {
  # Convert expression to text, preserving structure
  expr <- rlang::expr_text(expr)

  # Remove the tilde (~) if present
  expr <- gsub("^~\\s*", "", expr)

  # Replace R operators with Cypher operators
  expr <- stringr::str_replace_all(expr, stringr::fixed("%~%"), "=~")
  expr <- stringr::str_replace_all(expr, stringr::fixed("%in_list%"), "IN")
  expr <- stringr::str_replace_all(expr, stringr::fixed("=="), "=")
  expr <- stringr::str_replace_all(expr, stringr::fixed("!="), "<>")
  expr <- stringr::str_replace_all(expr, stringr::fixed("=="), "=")
  expr <- stringr::str_replace_all(expr, stringr::fixed("&"), "AND")
  expr <- stringr::str_replace_all(expr, stringr::fixed("|"), "OR")
  expr <- stringr::str_replace_all(expr, stringr::fixed("!"), "NOT")

  # Properly handle property referencing for equality
  # this looks for patterns like `property_name = *****`, where ***** can be anything; it becomes `n.property_name = *****`
  expr <- stringr::str_replace_all(expr, stringr::regex("\\b([a-zA-Z_][a-zA-Z0-9_]*)\\b(?=\\s*=)"), "n.\\1")

  # Handle IN where the property name should be on the right side
  ## pattern: `IN property_name` -> `IN n.property_name`
  expr <- stringr::str_replace_all(expr, stringr::regex("(IN\\s+)(\\b[a-zA-Z_][a-zA-Z0-9_]*\\b)"), "\\1n.\\2")

  # Handle string literals correctly
  expr <- stringr::str_replace_all(expr, "\"", "'")

  return(expr)
}


#' @importFrom rlang enquos
#' @importFrom purrr map_chr
generate_cypher_conditionals <- function(...) {
  # Capture the expressions
  conditions <- rlang::enquos(...)

  # Translate the conditions to Cypher syntax
  condition_strings <- purrr::map_chr(conditions, expr_to_cypher)

  query <- ifelse(length(condition_strings) > 0, paste(condition_strings, collapse = " AND "), "true")

  # Return the complete Cypher query
  return(query)
}

######### Exported Functions #########

#' @export
#' @import tidygraph
#' @import dplyr
#' @import stringr
fetch_nodes.neo4j_engine <- function(engine, ..., query_ids = NULL, page_size = 5000, limit = NULL) {
    if(!is.null(query_ids)) {
        # if query_ids is of length 1, we need to wrap it in a list for it to be sent as an array param
        if(length(query_ids) == 1) {
            query_ids <- list(query_ids)
        }
    	  query <- "MATCH (n) WHERE n.id IN $id"
    	  params <- list(id = query_ids)
    } else {
        query <- paste0("MATCH (n) WHERE (", generate_cypher_conditionals(...), ")")
        params <- list()
    }

		# cypher_query can't handle no-param param lists, replace with NULL if empty
		if(length(params) == 0) { params <- NULL}


		preflight_query <- paste0(query, " RETURN COUNT(n) as total_results")

		message("Fetching; counting matching nodes... ", appendLF = FALSE)
		preflight_result <- cypher_query_df(engine,
																				preflight_query,
																				parameters = params)

		total_results <- preflight_result$total_results


		message(" total: ", total_results, ".")
		if(!is.null(limit)) {
			if(limit < total_results) {
				warning("Specified limit (", limit, ") is less than total, returning first ", limit, " ordered by id.", immediate. = TRUE, call. = FALSE)
				total_results <- limit
			}
		}

		last_max_node_id <- ""
		result_cumulative <- tbl_kgx(nodes = data.frame())

		last_result_size <- -1
		total_nodes_fetched <- 0

		if(!is.null(limit)) {
			page_size <- min(page_size, limit)
		}

		while(last_result_size != 0) {
			result_query <- paste0(query, " AND n.id > $last_max_node_id RETURN n ORDER BY n.id ASC LIMIT $page_size")

			params$last_max_node_id <- last_max_node_id
			params$page_size <- page_size

			result <- cypher_query(engine, result_query, parameters = params)

			last_result_size <- nrow(nodes(result))

			if(last_result_size > 0) {
				total_nodes_fetched <- total_nodes_fetched + last_result_size
				last_max_node_id <- max(nodes(result)$id)

				suppressMessages(result_cumulative <- graph_join(result_cumulative, result), class = "message")
				message(paste("Fetching; fetched", total_nodes_fetched, "of", total_results))
			}

			if(!is.null(limit)) {
				limit <- limit - page_size
				page_size <- min(page_size, limit)
			}
		}

		attr(result_cumulative, "last_engine") <-engine

		return(result_cumulative)
}
