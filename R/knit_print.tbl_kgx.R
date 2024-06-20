clean_df <- function(df) {
	new_df <- list()
	for(colname in colnames(df)) {
		col_i <- df[[colname]]

		if(is.list(col_i)) {
			colname <- paste0(colname, " (list)")
		}

		contents <- lapply(col_i, function(cell_data) {
			paste(capture.output(dput(cell_data, control = "useSource")), collapse = "")
		}) |> unlist()

		new_df[[colname]] <- contents
	}

	as_tibble(new_df)
}


#' Specialized print function for KGX graphs in knitted documents
#'
#' @export
#' @import knitr
#' @importFrom kableExtra kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra column_spec
knit_print.tbl_kgx <- function(graph, ...) {
	g <- order_cols(graph)

	nodes_colnames <- colnames(nodes(g))
	nodes_kbl <- clean_df(nodes(g)) |> kable("html", escape = FALSE) |> kable_styling(fixed_thead = TRUE,
																																										bootstrap_options = c("striped", "hover", "condensed"))

	if("description" %in% nodes_colnames) {
		colnum <- seq_along(nodes_colnames)[nodes_colnames == "description"]
		nodes_kbl <- nodes_kbl |> column_spec(colnum, width_min = "300px")
	}
	if("synonym" %in% nodes_colnames) {
		colnum <- seq_along(nodes_colnames)[nodes_colnames == "synonym"]
		nodes_kbl <- nodes_kbl |> column_spec(colnum, width_min = "300px")
	}

	edges_kbl <- clean_df(edges(g)) |> kable("html", escape = FALSE) |> kable_styling(fixed_thead = TRUE,
																																										bootstrap_options = c("striped", "hover", "condensed"))

	knitr::asis_output(knitr::knit_child(text = c(
		'',
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
		'</div>',
		'###',
		''
	), envir = environment(), quiet = TRUE))
}
