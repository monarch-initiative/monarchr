#' Print function for KGs.
#'
#' Provides basic features over tidygraph print.
#'
#' @param g A graph
#' @param suppress_prefix A character vector of prefixes to suppress in the printed output
#' @param ... Additional arguments to be passed to the print method for tbl_graph
#' @return The graph
#' @export
#' @import dplyr
#' @import tidygraph
print.tbl_kgx <- function(x, suppress_prefix = NULL, ...) {
  nodes_df <- x %>% as_tibble(active = "nodes")
  edges_df <- x %>% as_tibble(active = "edges")

  if(!is.null(suppress_prefix)) {
    # only suppress for character or factor cols
    suppress_prefix <- suppress_prefix %>% as.character()
    nodes_df <- nodes_df %>% dplyr::mutate_all(~ ifelse(is.character(.), gsub(paste(suppress_prefix, collapse = "|"), "", .), .))
    edges_df <- edges_df %>% dplyr::mutate_all(~ ifelse(is.character(.), gsub(paste(suppress_prefix, collapse = "|"), "", .), .))
  }

  options(colorDF_n=5)
  library(tibble)
  options(pillar.width = c(id = 20))

  cat("Local Knowledge Graph. Nodes:\n")
    style <- list(interleave=NULL,
              # row.names=list(fg="#c7c7c7", bg="#343434"),
              # col.names=list(fg="#c7c7c7", bg="#343434"),
              # decoration=c(),
              col.styles = list(id = list(fg="#c46666", bg="#343434")))


    if(!is.null(attr(x, "last_engine"))) {
    	e <- attr(x, "last_engine")
    	# the engine preferences key node_property_priority specifies the preferred first order of node columns
    	prefs <- e$preferences$node_property_priority
    	# so we need to select those columns first (if they exist), then the rest
    	used_prop_names <- prefs[prefs %in% names(nodes_df)]
    	node_prop_names <- c(used_prop_names, setdiff(names(nodes_df), used_prop_names))
    	nodes_df <- nodes_df[, node_prop_names]
    }

    ndf <- nodes_df %>% colorDF::colorDF()
    # example of assigning multiple values in one assignment:
    colorDF::df_style(ndf) <- style
    print(ndf)

    cat("\n\nEdges:\n")
    edf <- edges_df %>% colorDF::colorDF()
    colorDF::df_style(edf) <- style
    print(edf)
}

