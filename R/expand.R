#' Expand a monarch_kg graph.
#' 
#' @description Expand a monarch_kg graph by adding nodes linked to the selected node set of the given types. 
#' All selected nodes will be expanded from, even if they are of different types. For example,
#' if the selected nodes include a disease and a gene, and the types include "Gene", then all
#' genes linked to the selected disease and all genes linked to the selected gene will be added
#' to the graph.
#' 
#' @param graph A monarch_kg graph object.
#' @param to_categories A character vector of entity types to expand to. If NULL (default), all types
#'  will be expanded to.
#' @param verbose A logical indicating whether to print progress messages. Default is FALSE.
#' @return A monarch_kg graph object.
#' @examples
#' # Expand a monarch_kg graph to all genes linked to the selected disease
#' graph <- seed_graph("MONDO:0005148")
#' graph <- expand(graph, to_categories = "Gene")
#' @export
#' @importFrom tidygraph activate filter
#' @importFrom dplyr left_join rename pull
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom progress progress_bar
expand <- function(graph, to_categories = NULL, verbose = FALSE) {
    selected_id_rows <- graph %>% 
        activate(nodes) %>% 
        filter(selected == TRUE) %>% 
        as.data.frame() %>%
        split(1:nrow(.))

    if(is.null(to_categories)) {
        to_categories <- get_entity_types()
    }
    print(association_categories_df)

    # so for each id:
       # for each source category:
           # for each destination category:
              # lookup associations
              # for each association type
                  # 

    str(selected_id_rows)

    # new_graphs <- lapply(selected_id_rows, function(id_row) {
    #     source_categories <- id_row$categories
    #     main_source_category <- source_categories[[1]]
    #     source_id <- id_row$id

    #     association_categories <- lapply(to_categories, function(type) {
    #         print(type)
    #         print(main_source_category)
    #         get_association_categories(main_source_category, type)
    #     })

    #     print(association_categories)
        
    #     # assoc_categories <- get_association_categories(main_source_category)

    # })

    # str(new_graphs)
    graph %>% activate(nodes) %>% as.data.frame()
}