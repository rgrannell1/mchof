
FLAG <- function (fmt = "still need to work on this feature", ...) {
	# a defensive function to prevent an unfinished file being shipped
	
	warning (sprintf(
		paste0(fmt, collapse = "\n"),...), call. = FALSE)
}

'%of%' <- function (f, g) {
	function (...) f(g(...))
}

'%throws%' <- function (bool, expr) {
	# general function, but used specifically for 
	# throwing exceptions. beware operator presidence
	
	stopifnot(is.logical(bool))
	
	if (bool) expr
}

is_boolean <- function (x) {
	# is the value true or false
	
	length(x) > 0 && is.logical(x) && !is.na(x)
}

stopf <- function (fmt, ...) {
	# combines stop, paste0 and sprintf
	
	stop (sprintf(
		paste0(fmt, collapse = "\n"),...), call. = FALSE)
}

warningf <- function (fmt, ..., call. = FALSE) {
	# combines stop, paste0 and sprintf
	
	warning (sprintf(
		paste0(fmt, collapse = "\n"),...), call.)
}

messagef <- function (fmt, ...) {
	# combines message, paste0 & sprintf
	
	message (sprintf(
		paste0(fmt, collapse = "\n"),...))
}
