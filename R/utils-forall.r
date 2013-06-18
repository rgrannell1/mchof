

ISSUE("fix cache assignment")
ISSUE("add expression -> func tool")
ISSUE("more rigourous checks, fix trycatch reports")


forall_cache <- list()

forall <- function (
	cases, expect, given = function (...) TRUE,
	opts = list(time = 4), info = '') {
	# a lightweight quickcheck function

	opts$time <-   getOption("forall_time", opts$time)
	opts$length <- getOption("forall_length", opts$length)
	
	(length(formals(expect)) != length(names(cases))) %throws% 
		messages$length_mismatch(
			"forall()", list(formals(expect), names(cases)),
			 "formals of expect", "names of cases")
	
	testargs_iter <- ihasNext(do.call(product, cases))
	
	predicate <- function (case) {
		# args -> boolean
		# does a set of testargs pass the specified test,
		# after composing given and expect into one function

		tryCatch(
			precondition_holds <- do.call(given, case),
			error = function (err) {

				#forall_cache <<- list(
				#	info = info,
				#	time = date(),
				#	cases = case
				#)

				stopf(c(
					"%s",
					"the precondition encountered an error",
					"the case which caused the error has been assigned to forall_cache"),
					info
				)
				stop(err)
			}
		)

		(!is_boolean(precondition_holds)) %throws% 
			messages$not_a_bool(
				"given(args)", precondition_holds,
				"the result of given")

		if (precondition_holds) {
			tests_ran <<- tests_ran + 1
			
			tryCatch(
				statement_holds <- do.call(expect, case),
				error = function (err) {

					#forall_cache <<- list(
					#	info = info,
					#	time = date(),
					#	cases = case
					#)

					stopf(c(
						"%s",
						"the expectation encountered an error",
						"the case which caused the error has been assigned to forall_cache"),
						info
					)
					stop(err)
				}
			)

			(!is_boolean(statement_holds)) %throws% 
				messages$not_a_bool(
					"given(args)", statement_holds,
					"the result of given")

			statement_holds
		} else TRUE

	}
	
	tests_ran <- 0 # side-effectfully updated
	results <- c()
	time_left <- stopwatch(opts$time)
	
	while (hasNext(testargs_iter) && time_left()) {
		
		args <- nextElem(testargs_iter)
		test_return_value <- predicate(args)
		
		(!is_boolean(test_return_value)) %throws% 
			messages$not_a_bool(
				"forall()", test_return_value, "result of expect")
		
		results <- c(
			results,
			list(list(
				passed = test_return_value,
				args = args
		)))
	}

	failed <- Filter(
		function (test) !test$passed,
		results
	)

	if (length(failed) > 0) {

		which_failed <- which(
			sapply(results, function (test) !test$passed)
		)

		forall_cache <<- list(
			info = info,
			time = date(),
			cases = list(
				Map(
					function (test) {
						test$args
					},
					results[which_failed])
			)
		)

		stopf(c(
			"failed! %s",
			"the test failed for the first time after %s tests",
			"%s tests met their precondition",
			"%s tests failed, %s tests passed",
			"",
			"a list of the cases which failed has been assigned to forall_cache"),
			info,
			min(which_failed), tests_ran,
			length(which_failed), length(results) - length(which_failed)
		)



	} else {
		messagef(
			'%s:\n\t true, passed %s tests (%s tests ran)',
			info, length(results), tests_ran)		
	}

}