
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

group_into <- function (x, size) {
	# groups x into chucks of size,
	# unless too few elements are left
	
	if (size == 1) {
		list(x)
	} else {	
		lapply(
			seq(from = 1, to = length(x), by = size),
			function (lower) {
				x[ lower:min(length(x), lower + size - 1) ]
		})
	}
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
