
#' @importFrom httr POST content http_status
#' @import tidygraph
#' @import dplyr
monarch_semsim <- function(graph1, graph2, metric = "ancestor_information_content") {
    engine <- monarch_engine()
    api_url <- paste0(engine$preferences$monarch_api_url, "/semsim/compare")

    subject_ids <- nodes(graph1)$id
    objects_ids <- nodes(graph2)$id

    params <- list(
        # use I() to prevent unboxing of length-1 vectors to scalars; see https://httr.r-lib.org/reference/POST.html
        "subjects" = I(subject_ids),
        "objects" = I(objects_ids),
        "metric" = metric
    )

    # this is a POST request
    response <- POST(api_url, body = params, encode = "json")

    # if the response is not 200, throw an error
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

    edges_df <- rbind(subject_best_matches, object_best_matches)

    mentioned_ids <- unique(c(subject_best_matches$subject, subject_best_matches$object, object_best_matches$subject, object_best_matches$object))

    nodes_df <- graph1 |>
        graph_join(graph2) |>
        nodes() |>
        filter(id %in% mentioned_ids)

    g <- tbl_kgx(nodes = nodes_df, edges = edges_df, attach_engine = monarch_engine())
    g
}