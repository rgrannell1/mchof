
'%throws%' <- function (bool, expr) {
	# general function, but used specifically for 
	# throwing exceptions
	
	if (bool) expr
}

is_boolean <- function (x) {
	# is the value true or false
	
	is.logical(x) && !is.na(x)
}
