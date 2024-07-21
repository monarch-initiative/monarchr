#' Check availability of Monarch Initiative API
#' 
#' Attempts to connect to the Monarch Initiative API and use the specified functionality. Returns FALSE if the API is not available the result is not as expected.
#' 
#' @param warn A logical indicating whether to print a warning message if with failure information if the database is not available or not properly formatted. Default is TRUE.
#' @param service The service to check: "search", "semsim", "graph", or a vector of these. Default is "graph".
#' @return TRUE if the available features are online, FALSE otherwise.
#' @export
#' @examples
#' print(monarch_engine_check())
#' 
monarch_engine_check <- function(warn = TRUE, service = "graph") {
	# this will throw an error if it cannot connect:
	# graph_conn <- neo2R::startGraph(url, username = username, password = password)

    # use try to see if we can successfully create a connection; return TRUE if successful, FALSE if not
	tryCatch({
        if("graph" %in% service) {
    		e <- monarch_engine()
    		# check to see if we can run a query, we'll just grab one random node; this should fail 
    		# if something is wrong (e.g. the database is not in KGX format)
    		cypher_query(e, "MATCH (n)-[r]->(q) RETURN n, r, q LIMIT 1")
    		return(TRUE)
        } else if("search" %in% service) {
            cf_hits <- monarch_search("Cystic fibrosis", category = "biolink:Disease", limit = 5)
            return(TRUE)
        } else if("semsim" %in% service) {
            phenos1 <- tbl_kgx(nodes = data.frame(id = c("HP:0000001", "HP:0000002", "HP:0000003")))
            phenos2 <- tbl_kgx(nodes = data.frame(id = c("HP:0000004", "HP:0000005", "HP:0000006")))
            semsim <- monarch_semsim(phenos1, phenos2)
            return(TRUE)
        } else {
            warning("Service must be one of 'search', 'semsim', or 'graph'")
            return(FALSE)
        }
	}, error = function(e) {
        if(warn) {
            warning(e$message)
        }
		return(FALSE)
	}, warning = function(e) {
        if(warn) {
            warning(e$message)
        }
        return(FALSE)
    })
}