
# given a dataframe with potential list columns,
# ensures that any NULL entries are replaced with a single NA
null_col_entries_to_na <- function(df) {
	for(name in names(df)) {
		col <- df[[name]]
		if(is.list(col)) {
			col <- lapply(col, function(el) {
													if(is.null(el)) {
														NA
													} else {
														el
													}
												}
			              )
			df[[name]] <- col
		}
	}

	return(df)
}

#' @import tidygraph
#' @import dplyr
#' @export
kg_join.tbl_kgx <- function(graph1, graph2, ...) {
    nodes_g1 <- nodes(graph1)
    nodes_g2 <- nodes(graph2)

    # we don't want to keep the to and from columns in the edges, since we'll be rebuilding edges from scratch,
    # and be running them through unique()
    edges_g1 <- edges(graph1) |> select(-to, -from)
    edges_g2 <- edges(graph2) |> select(-to, -from)

    all_nodes <- unique(nodes_g1 |>
                          full_join(nodes_g2)) |>
                 mutate(idx = row_number())  # add an index column to the nodes


    all_edges <- unique(edges_g1 |>
                          full_join(edges_g2))



    # given a single row of an edge data frame, we need to replicate it for each distinct from/to pair that match subjects and objects
    fill_edges <- function(row_df) {
        # we need to replicate this row with distinct from and to information from the all_nodes data
        subject_idxs <- all_nodes |> filter(id == row_df$subject) |> pull(idx)
        object_idxs <- all_nodes |> filter(id == row_df$object) |> pull(idx)
        # we need the cross-join of these two vectors
        cross <- expand.grid(from = subject_idxs, to = object_idxs)
        # now we need to add the properties from the original row to each of these new rows
        # we start by replcating the row_df nrow(cross) times
        row_df <- row_df[rep(1, nrow(cross)), ]
        # then we add the cross data to the row_df
        row_df <- cbind(row_df, cross)

        return(row_df)
    }

    filled_edges <- NULL

    # not using rowwise() here so that each row is nicely contained including list columns as a df row (not a list)
    # but maybe group_by() isn't right either - this triggers the call to fill_edges(.) even if there are no rows in the df
    # so we have a guard here
    if(nrow(all_edges) > 0) {
	    filled_edges <- all_edges |>
	      mutate(edge_idx = row_number()) |>
	      group_by(edge_idx) |>
	      do(fill_edges(.)) |>
	      as_tibble() |>
	      select(-edge_idx)  # remove the edge_idx column
    }

    all_nodes <- all_nodes |>
      select(-idx)  # remove the idx column

    all_nodes <- null_col_entries_to_na(all_nodes)
    filled_edges <- null_col_entries_to_na(filled_edges)
    res <- tbl_kgx(nodes = all_nodes, edges = filled_edges, attach_engine = get_engine(graph1, fail_if_missing = FALSE))
    return(res)
}
