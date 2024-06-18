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


order_nodes <- function(g) {
	e <- attr(g, "last_engine")
	node_prefs <- e$preferences$node_property_priority
	edge_prefs <- e$preferences$edge_property_priority

	current_node_names <- colnames(nodes(g))
	used_prefs_node_names <- node_prefs[node_prefs %in% current_node_names]
	set_node_names <- c(used_prefs_node_names, setdiff(current_node_names, used_prefs_node_names))

	current_edge_names <- colnames(edges(g))
	used_prefs_edge_names <- edge_prefs[edge_prefs %in% current_edge_names]
	set_edge_names <- c(used_prefs_edge_names, setdiff(current_edge_names, used_prefs_edge_names))

	res <- g |>
		activate(nodes) |>
		select(set_node_names) |>
		activate(edges) |>
		select(set_edge_names) |>
		activate(nodes)

	return(res)
}



#' @import kableExtra
#' @import stringr
clean_df <- function(df) {
	new_df <- list()
	for(colname in colnames(df)) {
		col_i <- df[[colname]]
		if(is.list(col_i)) {
			# list col
			colname <- paste0(colname, " (list)")

		}
		contents <- lapply(col_i, function(cell_data) {
			paste(capture.output(dput(cell_data, control = "useSource")), collapse = "")
			#paste(cell_data, collapse = "")
		}) |> unlist()

		#contents_trunc <- str_trunc(contents, 60)
		#new_df[[colname]] <- cell_spec(contents_trunc, "html", tooltip = contents)
		new_df[[colname]] <- contents
	}

	as_tibble(new_df)
}



#' @export
#' @import knitr
knit_print.tbl_kgx <- function(graph, ...) {
	g <- order_nodes(graph)
	nodes_kbl <- clean_df(nodes(g)) |> kable("html", escape = FALSE) |> kable_styling()
	edges_kbl <- clean_df(edges(g)) |> kable("html", escape = FALSE) |> kable_styling()
	knitr::asis_output(knitr::knit_child(text = c(
		'### {.tabset}',
		'',
		'#### Nodes',
		'<div style="max-height: 400px;overflow-y: auto;border-left: 1px solid #ddd;border-right:  1px solid #ddd;border-bottom: 1px solid #ddd;">',
		'```{r eval=TRUE, echo=FALSE}',
		'nodes_kbl',
		'```',
		'</div>',
		'#### Edges',
		'<div style="max-height: 400px;overflow-y: auto;border-left: 1px solid #ddd;border-right:  1px solid #ddd;border-bottom: 1px solid #ddd;">',
		'```{r eval=TRUE, echo=FALSE}',
		'edges_kbl',
		'```',
		'</div>'
	), envir = environment(), quiet = TRUE))
}
