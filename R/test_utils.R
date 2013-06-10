
# a placeholder testing function, until I write something 
# better. Used to test properties & lack of errors. Not very elegant, so
# I don't recommend that you use this code yourself. 

forall <- function (
	cases, expect, given = function (...) TRUE,
	opts = list(time = 4), info = '') {
	# a lightweight quickcheck function
	
	opts$time <- getOption("forall_time", opts$time)
	opts$length <- getOption("forall_length", opts$length)
	
	(length(formals(expect)) != length(names(cases))) %throws% stopf(
		c("mismatch between names for cases and expects names")	
	)
	
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
		
		(!is_boolean(test_return_value)) %throws% stopf(
			"%s didn't return t/f: actual value was %s",
			paste0(test_return_value, collapse = ", "))
		
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
					"the assertion wasn't true when %s were equal to..",
					"%s"),
				info, 
				paste0(names(formals(expect)), collapse = ', '),
				paste0(dput(test$args), collapse = ", "))
		},
		results)
	messagef(
		'%s:\n\t true, passed %s tests (%s tests ran)',
		info, length(results), tests_ran)
}


benchmark_code <- function (tests, len = 100, times = 2) {
	# compare the run-times of tests to controls

	report_result <- function (name, multiplier) {
		multiplier <- round(multiplier, 2)	
	
		messagef(
			"%s was %s times slower than the control test",
			name, paste(multiplier[1], "+-", multiplier[2]))
	}
	compare_results <- function (test, control) {
		# calculate how many times larger x is than y,
		# within a 95% confidence interval
		
		difference <-  median(test) / median(control)
		margin <- abs(
			( median(test) - sd(test) / median(control) + sd(control) ) /
			( median(test) + sd(test) / median(control) - sd(control) ))
			
		c(difference, margin)
	}
	
	timing <- Map(
		function (test) {
			
			x <- seq_len(len)
			
			list(
				name = test$name,
				test = microbenchmark(
					test$test(x),
					times = times)$time,
				control = microbenchmark(
					test$control(x),
					times = times)$time)
		},
		tests)
	
	Map(
		function (test) {
			report_result(test$name, compare_results(test$test, test$control))
		},
		timing)
	invisible(timing)
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


