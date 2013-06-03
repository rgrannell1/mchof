
require(microbenchmark)
require(mchof)

one_func <- function (...) 1
true_func <- function (...) TRUE
false_func <- Negate(true_func)
null_func <- function (...) NULL

fold_control <- function (x) Reduce(null_func, x)

quantifier_control <- filter_control <- 
	zip_control <- position_control <- function (x) {
	parallel::mclapply(x, null_func)
}

mchof_tests <- mcZipWith(
	function (x) {
		list(test = x[[1]], control = x[[2]], name = x[[3]])	
	},
	list(
		tests = list(
			function (x) mcAll(true_func, x),
			function (x) mcAny(false_func, x),
			function (x) mcFilter(true_func, x),
			function (x) mcFind(false_func, x),
			function (x) mcFold(one_func, 0, x),
			function (x) mcOne(false_func, x),
			function (x) mcPartition(true_func, x),
			function (x) mcPosition(false_func, x),
			function (x) mcReduce(one_func, x),
			function (x) mcReject(false_func, x),
			function (x) mcZipWith(true_func, x),
			function (x) mcUnzipWith(true_func, x)
		),
		controls = list(
			function (x) quantifier_control(x),
			function (x) quantifier_control(x),
			function (x) filter_control(x),
			function (x) position_control(x),
			function (x) fold_control(x),
			function (x) quantifier_control(x),
			function (x) filter_control(x),
			function (x) position_control(x),
			function (x) fold_control(x),
			function (x) filter_control(x),
			function (x) zip_control(x),
			function (x) zip_control(x)	
		),
		names = c("mcAll", "mcAny", "mcFilter", "mcFind",
		"mcFold", "mcOne", "mcPartition", "mcPosition",
		"mcReduce", "mcReject", "mcZipWith", "mcUnzipWith")
	)
)

test_backend <- function (x) call_mclapply(null_func, x)

backend_tests <- mcZipWith(
	function (x) {
		list(test = x[[1]], control = x[[2]], name = x[[3]])	
	},
	list(
		tests = list(
			function (x) parallel::mclapply(x, null_func),	
			function (x) Map(null_func, x),
			function (x) lapply(x, null_func)
		),
		controls = list(
			test_backend,
			test_backend,
			test_backend
		),
		names = c("mclapply", "map", "lapply")
	)
)

profile_tests <- function (tests, len = 100, times = 2) {
	# compare the run-times of the tests to the controls

	report_result <- function (name, multiplier) {
		multiplier <- round(multiplier, 2)	
	
		messagef(
			"%s was %s times slower than the control test",
			name, paste(multiplier[1], "±", multiplier[2]))
	}
	compare_results <- function (test, control) {
		# calculate how many times larger x is than y,
		# within a 95% confidence interval
		
		difference <-  median(test) / median(control)
		margin <- 0
		
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
					times = times)$time
			)
		},
		tests)
	
	Map(
		function (test) {
			report_result(test$name, compare_results(test$test, test$control))
		},
		timing)
	invisible(NULL)
}

options(mc.cores = NULL)
profile_tests(mchof_tests, len = 1000, 30)
profile_tests(backend_tests, len = 1000, 30)

