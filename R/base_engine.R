########### Internal functions ###########

#' @title base_engine
#' @description A base class for all engines
#' @param name A character string indicating the name of the engine.
#' @param preferences A named list of preferences for the engine.
#' @return An object of class `base_engine`
base_engine <- function(name = "default_engine", preferences = NULL, ...) {
    # read default prefs from the package
    default_preferences <- options("default_prefs")$default_prefs

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

    # now, if preferences is still not null, we want to override the default entries
    # that are provided in the preferences list
    # but just those, leaving other defaults in place
    if(!is.null(preferences)) {
        for(p in names(preferences)) {
            default_preferences[[p]] <- preferences[[p]]
        }
    }

    # now set preferences to the updated default preferences
    preferences <- default_preferences

    obj <- list(name = name,
                preferences = preferences)

    class(obj) <- c("base_engine", class(obj))
    return(obj)
}
