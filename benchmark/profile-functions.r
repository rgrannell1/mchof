
library (microbenchmark)
library (parallel)

options(mc.cores = NULL)

report_mchof_performance <- function (len, times) {
	# summarise the current performance of mchof,
	# including the performance of the backend
	
	test_vector <- seq_len(len)
	true_func <- function (...) TRUE
	one_func <- function (...) 1
	null_func <- function (...) NULL
	false_func <- Negate(true_func)
	
	ziplist <- list(
		seq_len(floor(len / 2)),
		(ceiling(floor(len / 2)) + 1):len)
	
	func_names <- c("mcAll", "mcAny", "mcFilter", "mcFind", 
		"mcFold", "mcOne", "mcPartition", "mcPosition",
		"mcReduce", "mcReject", "mcZipWith", "mcUnzipWith")
	
	profile_mchof <- function (len = 10, times = 1) {
		# return time data for every function in mchof,
		# for one value of len
	
		as.data.frame(structure(
			Map(
				function (test) {
					microbenchmark(test(), times = times)$time
				},
				list(
					function () mcAll(true_func, test_vector),
					function () mcAny(false_func, test_vector),
					function () mcFilter(true_func, test_vector),
					function () mcFind(false_func, test_vector),
					function () mcFold(one_func, 0, test_vector),
					function () mcOne(false_func, test_vector),
					function () mcPartition(true_func, test_vector),
					function () mcPosition(false_func, test_vector),
					function () mcReduce(one_func, test_vector),
					function () mcReject(false_func, test_vector),
					function () mcZipWith(true_func, ziplist),
					function () mcUnzipWith(true_func, ziplist))
			),
			names = func_names))
	}
	
	profile_controls <- function (len = 10, times = 1) {
		# return time data for similar functions, for one
		# value of len
		
		c("mcAll", "mcAny", "mcFilter", "mcFind", 
		"mcFold", "mcOne", "mcPartition", "mcPosition",
		"mcReduce", "mcReject", "mcZipWith", "mcUnzipWith")
		
		control_quantifier <- function (f, x) {
			mclapply(x, f)
		}
		control_zip <- function (f, x) {
			mclapply(x, f)
		}
		
		as.data.frame(structure(
			Map(
				function (test, profile_func) {
					microbenchmark(test(), times = times)$time
					},
				list(
					function () control_quantifier(true_func, test_vector),
					function () control_quantifier(true_func, test_vector),
					function () Filter(true_func, test_vector),
					function () Find(false_func, test_vector),
					function () Reduce(one_func, test_vector),
					function () control_quantifier(true_func, test_vector),
					function () Filter(true_func, test_vector),
					function () Position(false_func, test_vector),
					function () Reduce(one_func, test_vector),
					function () Filter(true_func, test_vector),
					function () control_zip(true_func, test_vector),
					function () control_zip(true_func, test_vector)	)),
			names = func_names))
	}
	profile_backend <- function (len = 10, times = 1) {
		list(
			map = microbenchmark(
				Map(null_func, seq_len(len)), times = times
			)$time,	
			call_mclapply = microbenchmark(
				call_mclapply(null_func, seq_len(len)), times = times
			)$time,
			mclapply = microbenchmark(
				mclapply(seq_len(len), null_func), times = times
			)$time,
			lapply = microbenchmark(
				lapply(seq_len(len), null_func), times = times
			)$time)
	
	}

	mchof_data <- profile_mchof(len, times)
	backend_data <- profile_backend(len, times)
	control_data <- profile_controls(len, times)
	
	mean_difference <- function (x, y) {
		
		c(1, 1)
		
	}
	
	backend_differences <- Map(
		function (val) {
			paste0(round(val[1], 4), '+-', round(val[2], 4), collapse = " ")
		},		
		list(mean_difference(
			backend_data$call_mclapply,
			backend_data$mclapply) / mean(backend_data$mclapply),
		mean_difference(
			backend_data$call_mclapply,
			backend_data$map) / mean(backend_data$map),
		mean_difference(
			backend_data$call_mclapply,
			backend_data$lapply) / mean(backend_data$lapply)	
	))

	messagef(
		"call_mclapply was %s times slower than mclapply( ), 
		%s times slower than Map( ) and %s times slower than lapply( )\n",
		backend_differences[[1]],
		backend_differences[[2]],
		backend_differences[[3]])
	
	messagef(c(unlist(Map(
		function (name, multipier, speed) {
			sprintf("%s was %s times slower than the control (%s seconds)",
				name, multipier, speed/10^9)
		},
		names(mchof_data),
		round(mchof_data / control_data, 0),
		round(mchof_data, 0)))))
	
	list(mchof = mchof_data, backend = backend_data, control = control_data)
}

report_mchof_performance(10, 10)
