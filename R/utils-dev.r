
ISSUE <- FLAG <- function (fmt = "still need to work on this feature", ...) {
	# a defensive function to prevent an unfinished file being shipped
	
	warning (sprintf(
		paste0(fmt, collapse = "\n"),...), call. = FALSE)

}

DONTRUN <- function (expr) {
	
}