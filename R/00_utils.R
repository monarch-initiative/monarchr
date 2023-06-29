#' Replace NULL with NA
#'
#' This function checks if an input is NULL, and if so, replaces it with NA
#' of a specific type. If the input is not NULL, the function simply returns the input.
#'
#' @param x A variable of any type. This is the variable that will be checked
#'   for NULL and possibly replaced with NA.
#' @param na_type_fun A function that converts NA to a specific type.
#'   This function should take NA as an input and return NA of a certain type.
#'   By default, this is `as.character`, which will return NA_character_.
#'   Other possibilities are `as.integer`, `as.numeric`, etc.
#'
#' @return The original variable if it is not NULL, or NA of a specific type
#'   if the original variable is NULL.
#'
#' @examples
#' null_to_na(NULL) # returns a character NA
#' null_to_na(NULL, as.integer) # returns an integer NA
#' null_to_na("Not null") # returns "Not null"
null_to_na <- function(x, na_type_fun = as.character) {
    if(is.null(x)) { return(na_type_fun(NA)) }
    else { return(x) }
}




#' Retrieve All Associations for a Given Entity in a Specified Category
#'
#' This function retrieves all associations for a given entity from the Monarch Initiative API within a specified category.
#' The result is a `monarch_kg` object representing a graph with the associations.
#'
#' @param category A character string specifying the association category. Example association categories are
#'   "biolink:CorrelatedGeneToDiseaseAssociation", "biolink:DiseaseToPhenotypicQualityAssociation", etc.
#' @param entity A character string specifying the Monarch Initiative entity id, e.g., "MONDO:0009061", "HGNC:1884", etc.
#' @param limit An integer specifying the maximum number of results to return. Useful for pagination.
#' @param offset An integer specifying the number of results to skip. Useful for pagination.
#'
#' @details This function is not exported and meant for internal use. As such it assumes that entity and category are valid identifiers, and will stop if not.
#'
#' @return A `monarch_kg` object representing a graph with the associations of the specified entity.
#'
#' @examples
#' # errors:
#' # get_association_all("sdf", "MONDO:0009061", 5, 1)
#' # get_association_all("biolink:CorrelatedGeneToDiseaseAssociation", "sdfsdf", 5, 1) # error
#'
#' # returns monarch_kg with 5 nodes
#' get_association_all("biolink:CorrelatedGeneToDiseaseAssociation", "HGNC:1884", 5, 1)
#'
get_association_all <- function(category, entity, limit, offset) {
    # TODO: remove offset and handle pagination here


    api_url <- paste0(getOption("monarchr.base_api_url"), "/association/all")

    params <- list(
        "category" = category,
        "entity" = entity,
        "limit" = limit,
        "offset" = offset
    )

    response <- memoized_get(api_url, query = params)
    if(response$status_code != 200) {
        stop(paste0("Status code not 200, stopping."))
    }

    response_content <- httr::content(response, "parsed")
    if(length(response_content$items) < 1) {
        stop(paste0("Entity or category unknown, stopping"))
    }

    edges <- lapply(response_content$items, function(item) {
            subject <- item$subject

            predicate <- item$predicate
            predicate_categories <- item$category
            if(!is.null(item$negated)) {
                # TODO: this is guess work, need to know what the possible values are here (NULL is one for sure)
                is_negated <- as.logical(item$negated)
            } else {
                is_negated <- FALSE
            }

            object <- item$object
            object_categories <- item$object_category
            object_label <- null_to_na(item$object_label)

            publications <- item$publications
            frequency_qualifier <- null_to_na(item$frequency_qualifier)
            onset_qualifier <- null_to_na(item$onset_qualifier)
            sex_qualifier <- null_to_na(item$sex_qualifier)
            # what are these?
            # .. ..$ source                     : NULL
            # .. ..$ stage_qualifier            : NULL
            # .. ..$ pathway                    : NULL
            # .. ..$ relation                   : NULL
            df <- tibble::tibble(subject = subject,
                                object = object,
                                predicate = predicate,
                                is_negated = is_negated,
                                predicate_categories = predicate_categories,
                                frequency_qualifier = frequency_qualifier,
                                onset_qualifier = onset_qualifier,
                                sex_qualifier = sex_qualifier)

            return(df)
        })

    edges <- do.call(rbind, edges)

    edges$from <- edges$subject
    edges$to <- edges$object

    entities <- unique(c(edges$subject, edges$object))
    pb <- progress::progress_bar$new(total = length(entities))
    message(paste0("Getting information from Monarch on ", length(entities), " entities."))
    nodes <- lapply(entities, function(entity) {
          pb$tick()
            get_entity(entity)
        })
    nodes <- do.call(rbind, nodes)

    graph <- tidygraph::as_tbl_graph(edges, directed = TRUE)
    graph <- dplyr::left_join(tidygraph::activate(graph, nodes), nodes, by = c("name" = "id"))
    graph <- dplyr::rename(tidygraph::activate(graph, nodes), id = name)

    return(as.monarch_kg(graph))
}



#' Get entity data from the Monarch Initiative API
#'
#' This function retrieves data for a given entity from the Monarch Initiative API.
#' If no entity is provided, or if the provided entity is not found in the API,
#' the function returns a dummy dataframe with the appropriate columns to become nodes
#' in a monarch_kg.
#'
#' @param entity A character string indicating the entity id. Default is NULL.
#'
#' @return A tibble with the retrieved entity data, or a dummy tibble with
#'   the correct columns but no data if the entity is NULL or not found.
#'
#' @examples
#' get_entity("MONDO:0005148")
#' get_entity() # returns an empty data tibble
#' get_entity("sdfsdf") # returns and empty tibble and produces a warning
get_entity <- function(entity = NULL) {
    dummy_df <- tibble::tibble(
        id = character(),
        categories = list(),
        label = character(),
        description = character(),
        symbol = character(),
        synonyms = list(),
        taxon = character(),
        inheritance_id = character(),
        inheritance_name = character()
    )

    if(is.null(entity)) { return(dummy_df) }

    api_url <- paste0(getOption("monarchr.base_api_url"), "/entity/", entity)

    #print(paste("Calling: ", httr::modify_url(api_url)))
    response <- memoized_get(api_url)

    if(response$status_code != 200) {
        warning(paste0("Entity unknown: ", entity, "\n", httr::content(response, "text")))
        return(dummy_df)
    }

    res <- httr::content(response, "parsed")

    df <- tibble::tibble(
        id = res$id,
        categories = res$category,
        label = null_to_na(res$name),
        description = null_to_na(res$description),
        symbol = null_to_na(res$symbol),
        synonyms = list(as.character(res$synonym)),
        taxon = null_to_na(res$taxon),
        inheritance_id = null_to_na(res$inheritance$id),
        inheritance_name = null_to_na(res$inheritance$id)
    )
    return(df)
}





#' Create a Single-Node Graph from a Monarch Initiative Entity ID
#'
#' This function retrieves data for a given entity from the Monarch Initiative API and
#' uses it to create a graph with a single node and no edges. If the entity is not found,
#' it returns an empty graph.
#'
#' @param entity_id A character string indicating the Monarch Initiative entity id,
#'   e.g. "MONDO:0009061" or "HGNC:1884". If NULL (default), an empty graph is returned.
#'
#' @return A `monarch_kg` graph object representing the entity, or an empty graph
#'   if the entity is not found.
#'
#' @examples
#' seed_graph("MONDO:0009061") # returns a monarch_kg object with one node corresponding to the entity
#' seed_graph() # returns an empty graph
#' seed_graph("sdflkjsdf")  # returns an empty graph and gives a warning
seed_graph <- function(entity_id = NULL) {
    # creates a graph with a single node and no edges, based on the entity_id, e.g. "MONDO:0009061" or "HGNC:1884".
    # If the entity is not found, returns an empty graph.
    entity <- get_entity(entity_id)
    entity$selected <- TRUE
    # is there really no better way to create a new graph with 0 edges?
    edges <- data.frame(to = entity$id, from = entity$id)
    graph <- tidygraph::as_tbl_graph(edges, directed = TRUE)
    graph <- dplyr::filter(tidygraph::activate(graph, edges), FALSE)
    graph <- dplyr::left_join(tidygraph::activate(graph, nodes), entity, by = c("name" = "id"))
    graph <- dplyr::rename(tidygraph::activate(graph, nodes), id = name)
    return(as.monarch_kg(graph))
}


memoized_get <- memoise::memoise(function(api_url, params = list()) {
    return(httr::GET(api_url, query = params))
})