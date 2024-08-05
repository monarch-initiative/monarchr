#' @import tidygraph
#' @import dplyr
#' @export
kg_join.tbl_kgx <- function(g1, g2) {
    nodes_g1 <- nodes(g1)
    nodes_g2 <- nodes(g2)

    # we don't want to keep the to and from columns in the edges, since we'll be rebuilding edges from scratch,
    # and be running them through unique()
    edges_g1 <- edges(g1) |> select(-to, -from)
    edges_g2 <- edges(g2) |> select(-to, -from)

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

    filled_edges <- all_edges |>
      mutate(edge_idx = row_number()) |>
      group_by(edge_idx) |>
      do(fill_edges(.)) |>
      as_tibble() |>
      select(-edge_idx)  # remove the edge_idx column

    all_nodes <- all_nodes |>
      select(-idx)  # remove the idx column

    res <- tbl_kgx(nodes = all_nodes, edges = filled_edges, attach_engine = get_engine(g1, fail_if_missing = FALSE))
    return(res)
}
