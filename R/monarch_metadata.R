#' @title Get association categories
#' @description Given two entity type labels, return the association categories that exist between them
#' @param type1 A character string indicating the first entity type
#' @param type2 A character string indicating the second entity type
#' @return A character vector of association categories
#' @details This function maps pairs of human-labels (e.g. "Gene" instead of "biolink:Gene") to the association categories
#' that connect them. Note that the order of the types does not matter, so get_association_categories("Gene", "Disease")
#' and get_association_categories("Disease", "Gene") will return the same result, even though the association is directional.
#' @examples
#' get_association_categories("Gene", "Disease")
#' get_association_categories("Disease", "Gene") # same result
get_association_categories <- function(type1 = "Gene", type2 = "Disease") {
    # Given two entity type labels, return the association categories that exist between them
    # for internal use, not exported
    type1 <- paste0("biolink:", type1)
    type2 <- paste0("biolink:", type2)
    # association_categories_df is a global set by 99_onload.R
    cats <- association_categories_df[(association_categories_df$subject_category == type1 & association_categories_df$object_category == type2) |
                                      (association_categories_df$object_category == type1 & association_categories_df$subject_category == type2), ]$association_category
    return(unique(cats))
}


#' @title Get entity types
#' @description Returns a character vector of potential human-readable entity types, e.g. "Gene", "Disease", etc.
#' @return A character vector of potential human-readable entity types
#' @details These are the corresponding biolink classes with the "biolink:" prefix removed (e.g. "biolink:Gene" -> "Gene")
#' @examples
#' get_entity_types()
get_entity_types <- function() {
    # returns a character vector of potential human-readable entity types, e.g. "Gene", "Disease", etc. 
    # these are the corresponding biolink classes with the "biolink:" prefix removed (e.g. "biolink:Gene" -> "Gene")
    entity_types <- as.list(unique(c(association_categories_df$subject_category, association_categories_df$object_category)))
    entity_types <- stringr::str_replace(entity_types, "biolink:", "")
    return(entity_types)
}