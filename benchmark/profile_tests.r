
require(microbenchmark)
require(mchof)

profile_tests <- function (tests, controls, names, len, times) {
	# compare the run-times of the tests to the controls

	pairs <- mcZip(tests, controls)
	
	timing <- Map(
		function (test, control) {
			
			list(
				test = microbenchmark(test(), times = times)$time
				control = microbenchmark(control(), times = times)$time)
		},
		tests, controls)
	
	results <- Map(
		function (test_pair) {
			compare_results(test_pair$test, test_pair$control)
		},
		timing)

	Map(
		function (test_name, result) {
			
			sprintf("%s was %s times faster that the control",
				test_name, result)
		},
		results, names)
	
}