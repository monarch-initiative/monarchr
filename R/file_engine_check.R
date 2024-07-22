#' Check availability of a file-based engine
#'
#' Attempts to connect to the specified file-based engine. Returns FALSE if the file is not available or not properly formatted.
#'
#' @param filename A character string indicating the path to the file-based engine.
#' @param warn A logical indicating whether to print a warning message if with failure information if the database is not available or not properly formatted. Default is TRUE.
#' @return TRUE if the database is available and properly formatted, FALSE otherwise.
#' @export
#' @examplesIf file_engine_check("https://kghub.io/kg-obo/sepio/2023-06-13/sepio_kgx_tsv.tar.gz")
#' print(file_engine_check("https://kghub.io/kg-obo/sepio/2023-06-13/sepio_kgx_tsv.tar.gz"))
#' print(file_engine_check("https://no-such-host.kghub.io/sepio_kgx_tsv.tar.gz"))
#' print(file_engine_check(system.file("extdata", "mondo_kgx_tsv.tar.gz", package = "monarchr")))
#' print(file_engine_check(system.file("extdata", "nosuch_kgx_tsv.tar.gz", package = "monarchr")))
file_engine_check <- function(filename, warn = TRUE) {
    # use try to see if we can successfully create a connection; return TRUE if successful, FALSE if not
	tryCatch({
		e <- file_engine(filename)
		return(TRUE)
	}, error = function(e) {
		if(warn) {
            warning(e$message)
        }
		return(FALSE)
	}, warning = function(e) {
        if(warn) {
            warning(e$message)
        }
        return(FALSE)
    })
}
