# we override the default print() function for tidygraphs, 
# only so that we can add an option of e.g. suppress_prefix = c("biolink:", "GO:"),
# which will remove those prefixes from the printed output in all character
# columns. This is useful for making the printed output more readable.
#
#' @param g A graph
#' @param suppress_prefix A character vector of prefixes to suppress in the printed output
#' @param ... Additional arguments to be passed to the print method for tbl_graph
#' @return The graph
#' @export
#' @examples
#' g <- monarch_search("fanconi anemia")
#' print(g)
#' print(g, suppress_prefix = c("biolink:", "GO:"))
#' @importFrom dplyr mutate_all
print.tbl_kgx <- function(x, suppress_prefix = c("biolink:"), ...) {
  nodes_df <- x %>% activate(nodes) %>% as_tibble()
  edges_df <- x %>% activate(edges) %>% as_tibble()

  if(!is.null(suppress_prefix)) {
    # only suppress for character or factor cols
    suppress_prefix <- suppress_prefix %>% as.character()
    nodes_df <- nodes_df %>% mutate_all(~ ifelse(is.character(.), gsub(paste(suppress_prefix, collapse = "|"), "", .), .))
    edges_df <- edges_df %>% mutate_all(~ ifelse(is.character(.), gsub(paste(suppress_prefix, collapse = "|"), "", .), .))
  }

  options(colorDF_n=5, width = 120)
  library(tibble)
#   options(pillar.width = c(id = 20))

  cat("Knowledge Graph. Nodes:\n")
    style <- list(interleave=NULL,
              #row.names=list(fg="#c7c7c7", bg="#343434"), 
              #col.names=list(fg="#c7c7c7", bg="#343434"), 
              #decoration=c(),
              col.styles = list(id = list(fg="#c46666", bg="#343434")))

    ndf <- nodes_df %>% colorDF::colorDF()
    # example of assigning multiple values in one assignment:
    colorDF::df_style(ndf) <- style
    print(ndf)

    cat("\n\nEdges:\n")
    edf <- edges_df %>% colorDF::colorDF()
    colorDF::df_style(edf) <- style
    print(edf)
}



#library(tidygraph)
#g <- query_ids(c("MONDO:0015780", "HP:0001903", "HGNC:2890"))
#g <- fetch_edges(g)
print.tbl_kgx(g)



