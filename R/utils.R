
'%throws%' <- function (bool, expr) {
	# general function, but used specifically for 
	# throwing exceptions. beware operator presidence
	
	if (bool) expr
}

is_boolean <- function (x) {
	# is the value true or false
	
	length(x) > 0 && is.logical(x) && !is.na(x)
}
