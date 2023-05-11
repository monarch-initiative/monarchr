#' Print a greeting and return a value
#'
#' This function prints a greeting message and returns the input value.
#'
#' @param x The value to be returned.
#' @return The input value.
#' @examples
#' hello("John")
#' # Prints: "Hello, world!"
#' # Returns: "John"
#' hello(42)
#' # Prints: "Hello, world!"
#' # Returns: 42
#' @export
hello <- function(x) {
  print("Hello, world!")
	return(x)
}
