#' Normalize Categories
#'
#' This function takes a list of vectors of categories and an ordered preference list over categories.
#' It selects the most preferred category from each vector, or the first category if no preferred categories are included.
#'
#' @param cats_list A list of vectors of categories.
#' @param cats_prefs An ordered preference list over categories.
#' @return A vector of normalized categories.
#' @examples
#' categories_list <- list(c("A", "B", "C"), c("D", "E", "F"))
#' categories_prefs <- c("B", "E", "A", "D", "C", "F")
#' normalize_categories(categories_list, categories_prefs)
normalize_categories <- function(cats_list, cats_prefs) {
	normed <- unlist(lapply(cats_list, function(categories) {
		positions <- match(categories, cats_prefs)

		# If all matches are NO, there is no preference match, so use the first label;
		# otherwise, use the label with the minimum position in the preferred list
		most_preferred_label <- if (all(is.na(positions))) {
			categories[1]
		} else {
			categories[which.min(positions)]
		}

		most_preferred_label
	}))

	return(normed)
}