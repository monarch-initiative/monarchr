# for array params in httr queries
# from https://stackoverflow.com/a/72532186
flatten_body_for_httr <- function(x) {
	# A form/query can only have one value per name, so take
	# any values that contain vectors length >1 and
	# split them up
	# list(x=1:2, y="a") becomes list(x=1, x=2, y="a")
	if (all(lengths(x) <= 1)) return(x);
	do.call("c", mapply(function(name, val) {
		if (length(val)==1 || any(c("form_file", "form_data") %in% class(val))) {
			x <- list(val)
			names(x) <- name
			x
		} else {
			x <- as.list(val)
			names(x) <- rep(name, length(val))
			x
		}
	}, names(x), x, USE.NAMES = FALSE, SIMPLIFY = FALSE))
}
