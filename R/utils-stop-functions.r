
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

'%throws%' <- function (bool, expr) {
	# general function, but used specifically for 
	# throwing exceptions. beware operator presidence
	
	stopifnot(is.logical(bool))
	
	if (bool) expr
}