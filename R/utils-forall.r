
forall <- function (
	cases, expect, given = function (...) TRUE,
	opts = list(time = 4), info = '') {

	has_generators <- any(sapply(
		cases,
		function (el) {
			is(el[[1]], 'generator')
		}
	))

	opts$time <-   getOption("forall_time", opts$time)
	
	(length(formals(expect)) != length(names(cases))) %throws% 
		messages$length_mismatch(
			"forall()", list(formals(expect), names(cases)),
			 "formals of expect", "names of cases")

	enumerator <- if (has_generators) {
		ihasNext( recycle(do.call(product, cases)) )
	} else {
		ihasNext(do.call(product, cases))
	}

	predicate <- function (case) {
		# args -> boolean
		# does a set of testargs pass the specified test,
		# after composing given and expect into one function

		tryCatch(
			precondition_holds <- do.call(given, case),
			error = function (err) {

				stopf(c(
					"%s",
					"\t\tthe precondition encountered an error:",
					"",
					"\t\t%s",
					"",
					"\t\tthe case which caused the error has been assigned to forall_cache"),
						info, err$message
				)
				stop(err)
			}
		)

		(!is_boolean(precondition_holds)) %throws% 
			messages$not_a_bool(
				"given(args)", precondition_holds,
				"the result of given")

		if (precondition_holds) {
			expectation_checked <<- expectation_checked + 1
			
			tryCatch(
				expectation_holds <- do.call(expect, case),
				error = function (err) {
	
					stopf(c(
						"%s",
						"\t\tthe expectation encountered an error:",
						"",
						"\t\t%s",
						"",
						"\t\tthe case which caused the error has been returned invisibly"),
						info, err$message
					)
				}
			)

			(!is_boolean(expectation_holds)) %throws% 
				messages$not_a_bool(
					"given(args)", expectation_holds,
					"the result of given")

			expectation_holds
		} else TRUE

	}
	
	expectation_checked <- 0 # side-effectfully updated
	time_left <- stopwatch(opts$time)

	should_run <- function () {
		hasNext(enumerator) && time_left()
	}

	results <- accWhile(
		function (previous_results) {

			if (should_run()) {

				testargs <- nextElem(enumerator)

				if (has_generators) {
					testargs <- Map(
						function (el) {

							if (is(el, "generator")) {
								el$f()
							} else el
						},
						testargs)
				}

				test_return_value <- predicate(testargs)

				list(
					passed = test_return_value,
					args = unname( testargs ))

			} else NULL
		}
	)

	failed <- Filter(
		function (test) {
			!test$passed
		},
		results
	)

	if (length(failed) > 0) {

		which_failed <- which(
			sapply(results, function (test) !test$passed)
		)

		on.exit(return(invisible(failed_info)), add = TRUE)

		failed_info <- list(
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
			"\t\tthe test failed for the first time after %s tests",
			"\t\t%s tests met their precondition",
			"\t\t%s tests failed, %s tests passed",
			"",
			"\t\tthe case which caused the error has been returned invisibly"),
			info,
			min(which_failed), expectation_checked,
			length(which_failed), length(results) - length(which_failed)
		)

	} else {
		messagef(
			c("%s",
			"\t\ttrue, passed %s tests (%s tests ran)"),
			info, length(results), expectation_checked)		
	}
	invisible(results)
}
