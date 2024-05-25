#' @title base_engine
#' @description A base class for all engines
#' @param name A character string indicating the name of the engine.
#' @param preferences A named list of preferences for the engine.
#' @return An object of class `base_engine`
#' @export
#' @examples
#' base_engine()
#' base_engine(name = "test")
#' base_engine(preferences = "preferences.yaml")
base_engine <- function(name = "default_engine", preferences = NULL, ...) {
    if(!is.null(preferences)) {
        # if preferences is a length-1 character vector ending with .yaml, and the file exists, read it
        if(is.character(preferences) && 
           length(preferences) == 1 && 
           grepl("\\.yaml$", preferences) && 
           file.exists(preferences)) {
                preferences <- yaml::read_yaml(preferences)

        # if it's a list, just use it
        } else if(is.list(preferences)) {
            preferences <- preferences
        }

    }

    # if preferences is NULL, read it from the package
    if(is.null(preferences)) {
        preferences <- options("default_prefs")$default_prefs
    }

    obj <- list(name = name,
                preferences = preferences)

    class(obj) <- c("base_engine", class(obj))
    return(obj)
}
