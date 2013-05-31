
# a placeholder testing function, until I write something 
# better. Used to test properties & lack of errors. Not very elegant, so
# I don't recommend that you use this code yourself. 



forall <- function (
	cases, expect, given = function (...) TRUE,
	opts = list(time = 2), info = '') {
	# a lightweight quickcheck function
	
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
	
	testargs_iter <- ihasNext(do.call(product, cases))
	
	predicate <- function (args) {
		if (do.call(given, args)) {
			tests_ran <<- tests_ran + 1
			do.call(expect, args)
		} else TRUE
	}
	
	tests_ran <- 0 # side-effectfully updated
	results <- c()
	time_left <- stopwatch(opts$time)
	
	while (hasNext(testargs_iter) && time_left()) {
		
		args <- nextElem(testargs_iter)
		test_return_value <- predicate(args)
		
		if (!is_boolean(test_return_value)) {
			stop(args, " didn't return t/f: actual value was ", test_return_value)
		}
		results <- c(
			results,
			list(list(
				passed = test_return_value,
				args = args
		)))
	}
	
	Map(
		function (test) {			
			(!test$passed) %throws% stopf(
				c(
					'failed! %s',
					"the assertion wasn't true when %s were equal to %s"),
				info, 
				paste0(names(formals(expect)), collapse = ', '),
				paste0(deparse(test$args), collapse = ', '))
		},
		results)
	messagef(
		'%s:\n\t true, passed %s tests (%s tests ran)',
		info, length(results), tests_ran)
}

all_equal <- function (x) {
	length(unique(x)) == 1	
}
