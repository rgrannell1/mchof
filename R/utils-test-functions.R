
# a placeholder testing function, until I write something 
# better. Used to test properties & lack of errors. Not very elegant, so
# I don't recommend that you use this code yourself. 

stopwatch <- function (seconds) {
	# returns a function with Sys.time( ) 
	# captured in a closure
	
	stopifnot (is.numeric(seconds) && seconds > 0)
	
	( function () {
		start_time <- Sys.time()
		function () {
			time_passed <- as.numeric(difftime( Sys.time(), start_time ))
			time_passed < seconds
		}		
	} )()
}

all_equal <- function (x) {
	length(unique(x)) == 1	
}

adapt_call <- function (func, with) {
	
	stopifnot(length(names(with)) == length(with))

	func_formals <- names(formals(func))
	call_with <- with[
		which(names(with) %in% func_formals)
	]
	do.call(func, call_with)
}

accWhile <- function (func) {
	# ( [a] -> NULL | [b] ) -> [b]
	# accumulates a list of the return values
	# of func in a sublist, until func returns NULL.

	acc <- list()
	res <- func (acc)

	while (!is.null(res)) {
		acc <- c(acc, list(res))
		res <- func(acc)
	}
	acc
}

