
#' Save a graph as a KGX-formatted .tar.gz file.
#'
#' Given a graph, saves it using the tabular KGX format
#' (see https://github.com/biolink/kgx/blob/master/specification/kgx-format.md) for later
#' use with `load_kgx()` (or even backing an engine with `file_engine()`). Note that if
#' any engine is associated with the graph it is not saved.
#'
#' @param graph A `tbl_kgx` graph to save.
#' @param filename File to save the graph to. Must end in .tar.gz.
#' @param ... Other parameters (unused)
#' @importFrom readr write_tsv
#' @importFrom archive archive_write_files
#' @return The input graph (invisibly).
#' @export
#' @examplesIf monarch_engine_check()
#' phenos <- monarch_engine() |>
#'           fetch_nodes(query_ids = "MONDO:0007525") |>
#'           expand(predicates = "biolink:has_phenotype",
#'                  categories = "biolink:PhenotypicFeature")
#'
#' save_kgx(phenos, "phenos.tar.gz")
#'
#' # when loading the graph, we can optionally attach an engine
#' loaded_phenos <- load_kgx("phenos.tar.gz", attach_engine = monarch_engine())
#'
#' loaded_phenos
#'
#' # cleanup saved file
#' file.remove("phenos.tar.gz")
save_kgx <- function(graph, filename = "saved_kgx_graph.tar.gz", ...) {
	# ensure that files is a .tar.gz
	if(!grepl(".tar.gz$", filename)) {
		stop("Filename must end in .tar.gz")
	}

	node_df <- nodes(graph)
	edge_df <- edges(graph)

	node_df <- node_df[,colnames(node_df)[colnames(node_df) != "pcategory"]]

	pipe_format <- function(df) {
		df |>
			# list columns need to be converted to character cols, with |-separated entries
			lapply(function(col) {
				if(is.list(col)) {
					# both empty and NA values should be empty strings
					fix <- col |> lapply(function(vec) {
						if(length(vec) == 0) {
							""
						} else {
							vec[is.na(vec)] <- ""
							paste0(vec, collapse = "|")
						}
					})
					# str(fix)
					fix |> as.character()
				} else {
					col[is.na(col)] <- ""
					as.character(col)
				}
			}) |>
			as.data.frame()
	}

	node_df <- pipe_format(node_df)
	edge_df <- pipe_format(edge_df)

	basename <- stringr::str_replace(filename, ".tar.gz", "")

	node_df_file <- paste0(basename, "_nodes.tsv")
	edge_df_file <- paste0(basename, "_edges.tsv")

	write_tsv(node_df, node_df_file, col_names = TRUE)
	write_tsv(edge_df, edge_df_file, col_names = TRUE)

	archive_write_files(filename, c(node_df_file, edge_df_file))

	file.remove(node_df_file)
	file.remove(edge_df_file)

	return(invisible(graph))
}
