#' Infix regular expression match.
#'
#' An infix alias for stringr::str_detect().
#'
#' @examples
#' rownames(mtcars) %~% "^Merc"
#'
#' @param string A character vector to look for matches in.
#' @param pattern A regular expression to look for.
#' @return A logical vector indicating pattern matches.
#' @export
#' @importFrom stringr str_detect
`%~%` <- function(string, pattern) {
	str_detect(string, pattern)
}
