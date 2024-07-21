#' Semantic similarity mapping between two graphs
#' 
#' This function calls the Monarch-hosted semantic similarity API to compare two
#' graphs, via the same endpoints as the Monarch Phenotype Explorer:
#' https://monarchinitiative.org/explore#phenotype-explorer.
#' 
#' The API returns the best matches between the nodes of the two graphs, based on
#' a specified knowledge-graph-boased metric: the default is `"ancestor_information_content"`, 
#' also available are `"jaccard_similarity"` and `"phenodigm_score"`. The result is
#' returned as a graph, with `"computed:best_matches"` edges between the nodes of the two input graphs.
#' 
#' By default, the function only returns the best matches from the first graph to the second graph, and
#' removes any nodes that do not have a match. If `include_reverse = TRUE`, the function also returns
#' the best matches from the second graph to the first graph.
#' 
#' The engine attached to the return graph is that of the query.
#' 
#' @param query_graph A tbl_kgx graph.
#' @param target_graph A tbl_kgx graph.
#' @param metric The semantic similarity metric to use. Default is `"ancestor_information_content"`. Also available are `"jaccard_similarity"` and `"phenodigm_score"`.
#' @param include_reverse Whether to include the best matches from the target graph to the query graph. Default is `FALSE`.
#' @param keep_unmatched_targets Whether to keep nodes in the target graph that do not have a match. Default is `FALSE`.
#' @return A tbl_kgx graph with `"computed:best_matches"` edges between the nodes of the two input graphs.
#' @export
#' 
#' @importFrom httr POST content http_status
#' @import tidygraph
#' @import dplyr
#' @examplesIf monarch_engine_check(service = "semsim")
#'
#' g1 <- monarch_engine() |>
#'   fetch_nodes(query_ids = "MONDO:0007947") |>
#'   expand(categories = "biolink:PhenotypicFeature")
#'
#' g2 <- monarch_engine() |>
#'   fetch_nodes(query_ids = "MONDO:0007522") |>
#'   expand(categories = "biolink:PhenotypicFeature")
#'
#' sim <- monarch_semsim(g1, g2)
#' print(sim)
#'
#' # also inclue the unmatched targets
#' sim <- monarch_semsim(g1, g2, keep_unmatched = TRUE)
#' print(sim)
#' 
#' # inclue reverse matches
#' sim <- monarch_semsim(g1, g2, include_reverse = TRUE)
#' print(sim)
#' 
monarch_semsim <- function(query_graph, 
                           target_graph, 
                           metric = "ancestor_information_content", 
                           include_reverse = FALSE, 
                           keep_unmatched = FALSE) {
    # check that the metric is valid
    assert_that(metric %in% c("ancestor_information_content", "jaccard_similarity", "phenodigm_score"), msg = "metric must be one of 'ancestor_information_content', 'jaccard_similarity', or 'phenodigm_score'")

    engine <- monarch_engine()
    api_url <- paste0(engine$preferences$monarch_api_url, "/semsim/compare")

    # these are called subject_ids and object_ids in the API, but 
    # these don't relate to "subject" and "object" of a predicate
    # we use the APIs terminology here
    subject_ids <- nodes(query_graph)$id
    objects_ids <- nodes(target_graph)$id

    params <- list(
        # use I() to prevent unboxing of length-1 vectors to scalars; see https://httr.r-lib.org/reference/POST.html
        "subjects" = I(subject_ids),
        "objects" = I(objects_ids),
        "metric" = metric
    )

    response <- POST(api_url, body = params, encode = "json")

    if(response$status_code != 200) {
        stop(paste0("Error: ", response$status_code, " ", http_status(response$status_code)$message))
    }

    response_content <- content(response, "parsed")

    # the result will have:
    # $subject_best_matches
    #   -> named list with names being subject IDs, sublist entries of interest:
    #      $match_target: the best match target ID
    #      $score: the similarity score
    #      $similarity: a list with an $ancestor_id entry that might be of use
    # $object_best_matches
    #   -> named list with names being object IDs, same as above

    # parse subject best matches
    subject_best_matches <- response_content$subject_best_matches
    subject_best_matches <- lapply(seq_along(subject_best_matches), function(i) {
        x <- subject_best_matches[[i]]
        name <- names(subject_best_matches)[i]
        data.frame(
            subject = name,
            predicate = "computed:best_matches",
            object = x$match_target,
            metric = metric,
            score = x$score,
            ancestor_id = x$similarity$ancestor_id
        )
    })

    subject_best_matches <- do.call(rbind, subject_best_matches)
    keep_edges_df <- subject_best_matches

    if(include_reverse) {
        # parse object best matches
        object_best_matches <- response_content$object_best_matches
        object_best_matches <- lapply(seq_along(object_best_matches), function(i) {
            x <- object_best_matches[[i]]
            name <- names(object_best_matches)[i]
            data.frame(
                subject = name,
                predicate = "computed:best_matches",
                object = x$match_target,
                metric = metric,
                score = x$score,
                ancestor_id = x$similarity$ancestor_id
            )
        })

        object_best_matches <- do.call(rbind, object_best_matches)
        keep_edges_df <- rbind(keep_edges_df, object_best_matches)
    }

    if(keep_unmatched) {
        mentioned_ids <- unique(c(nodes(query_graph)$id, nodes(target_graph)$id))
    } else {
        mentioned_ids <- unique(c(keep_edges_df$subject, keep_edges_df$object))
    }

    nodes_df <- query_graph |>
        graph_join(target_graph) |>
        nodes() |>
        filter(id %in% mentioned_ids)

    g <- tbl_kgx(nodes = nodes_df, edges = keep_edges_df, attach_engine = get_engine(query_graph, fail_if_missing = FALSE))

    return(g)
}