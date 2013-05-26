
pick_one <- function (x) {
	sample(x, size = 1)
}
pick_size <- function (n) {
	sample(seq_len(n), size = 1)
}
rep_sample <- function (x, n) {
	sample(x, size = n, replace = TRUE)
}
repl <- function (n, expr) {
	replicate(n, expr, FALSE)
}

# so. many. magic. numbers !!

rInputs <- list(
	integers = function (n) {
		rep_sample(-1000:1000, n)
	},
	floats = function (n) {
		round( runif(n, -1000, +1000), 2)	
	},
	letters = function (n) {
		rep_sample(letters[1:26], n)	
	},
	strings = function (n) {
		repl(n, ( function () {		
			list( paste0(rInputs$letters( pick_size(20) ), collapse = '') )
		} )())
	},
	booleans = function (n) {
		rep_sample(c(TRUE, FALSE), n)		
	},
	logicals = function (n) {
		rep_sample(c(TRUE, FALSE, NA),  n)
	},
	null = function (n) {
		NULL
	},
	
	character_vectors = function (n) {
		repl(n, list( rInputs$strings( pick_size(20) ) ))
	},
	integer_vectors = function (n) {
		repl(n, list( rInputs$integers( pick_size(20) ) ))
	},
	float_vectors = function (n) {
		repl(n, list( rInputs$floats( pick_size(20) ) ))
	},

	list_cells = function (n) {
		repl(n, {
			func <- pick_one(list(
				rInputs$integers, rInputs$floats, 
				rInputs$letters,
				rInputs$booleans, rInputs$logicals,
				rInputs$null))[[1]]
							 
			list ( func(pick_size(20)) )
		})
	},
	flatlist = function (n) {
		repl(n, rInputs$list_cells( pick_size(20) ))
	},
	paropts = function (n) {
		repl(n, {
			cores <- pick_size(5)
			list(mc.cores = cores)
		})
	}
)

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
		if (do.call(given, args)) do.call(expect, args) else TRUE
	}
	
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
	messagef('okay, passed %s tests', length(results))
}
