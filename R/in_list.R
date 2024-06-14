#' Check if an element is contained in list sub-elements
#' 
#' This function is useful for filtering dataframes by list columns. It checks if an element is contained in each list element, returning a logical vector. Note
#' that this is different than `%in%` which does not work with list columns in a natural manner.
#' 
#' @examples
#' library(dplyr)
#' 
#' df <- tibble(id = c("A", "B", "C"), list_col = list(1:3, 2:4, 3:5))
#' 
#' # works naturally for list columns
#' df %>% filter(2 %in_list% list_col)
#' 
#' # also works with basic vector columns
#' df %>% filter("B" %in_list% id)
#' 
#' # does not work with multi-valued left hand sides
#' # df %>% filter(c("B", "C") %in_list% id) # error
#' 
#' @param element The element to check for
#' @param list A list of elements to check against
#' @return A logical vector indicating if the element is in each list element
#' @export
`%in_list%` <- function(element, list) {
  vapply(list, function(x) element %in% x, logical(1))
}