
library (microbenchmark)

report_mchof_performance <- function (len, times) {
	# summarise the current performance of mchof,
	# including the performance of the backend
	
	test_vector <- seq_len(len)
	true_func <- function (x) TRUE
	false_func <- Negate(true_func)
	
	ziplist <- list(
		seq_len(floor(len / 2)),
		(ceiling(floor(len / 2)) + 1):len)
	
	profile_mchof <- function (len = 10, times = 1) {
		# return time data for every function in mchof,
		# for one value of len
	
		as.data.frame(structure(
			Map(
				function (test) {
					timing <- microbenchmark(test())$time
					c(
						median = median(timing),
						iqr = IQR(timing),
						iqr_coeff = round(IQR(timing)/median(timing), 2))
				},
				list(
					function () mcAll(true_func, test_vector),
					function () mcAny(false_func, test_vector),
					function () mcFilter(true_func, test_vector),
					function () mcFind(false_func, test_vector),
					function () mcFold(function (a, b) 1, 0, test_vector),
					function () mcOne(false_func, test_vector),
					function () mcPartition(true_func, test_vector),
					function () mcPosition(false_func, test_vector),
					function () mcReduce(function (a, b) 1, test_vector),
					function () mcReject(false_func, test_vector),
					function () mcZipWith(true_func, ziplist),
					function () mcUnzipWith(true_func, ziplist))
			),
			names = c("mcAll", "mcAny", "mcFilter", "mcFind", 
				"mcFold", "mcOne", "mcPartition", "mcPosition",
				"mcReduce", "mcReject", "mcZipWith", "mcUnzipWith")))
	}
	
	profile_controls <- function (len = 10, times = 1) {
		# return time data for similar functions, for one
		# value of len
		
		as.data.frame(structure(
			Map(
				function (test, profile_func) {
					timing <- microbenchmark(test())$time
					c(
						median = median(timing),
						iqr = IQR(timing),
						iqr_coeff = round(IQR(timing)/median(timing), 2))	
				},
				list(
					function () Map(function (x) TRUE, test_vector),
					function () Map(function (x) TRUE, test_vector),
					function () Filter(true_func, test_vector),
					function () Find(false_func, test_vector),
					function () Reduce(function (a, b) 1, test_vector),
					function () Map(function (x) TRUE, test_vector),
					function () Filter(true_func, test_vector),
					function () Position(false_func, test_vector),
					function () Reduce(function (a, b) 1, test_vector),
					function () Filter(true_func, test_vector),
					function () Map(function (x) TRUE, test_vector),
					function () Map(function (x) TRUE, test_vector)	
				)),
			names = c("mcAll", "mcAny", "mcFilter", "mcFind", 
				"mcFold", "mcOne", "mcPartition", "mcPosition",
				"mcReduce", "mcReject", "mcZipWith", "mcUnzipWith")))
	}
	
	timing <- list(
		map = microbenchmark(
			Map(identity, seq_len(len)),
			times = times
		)$time,	
		call_mclapply = microbenchmark(
			call_mclapply(identity, seq_len(len)),
			times = times
		)$time,
		mclapply = microbenchmark(
			call_mclapply(identity, seq_len(len)),
			times = times
		)$time,
		lapply = microbenchmark(
			lapply(seq_len(len), identity),
			times = times
		)$time)

	medians <- c(
		map = median(timing$map),
		mclapply = median(timing$mclapply),
		call_mclapply = median(timing$call_mclapply),
		lapply = median(timing$lapply))

	messagef(
		"call_mclapply was %s times slower than mclapply( ), 
		%s times slower than Map( ) and %s times slower than lapply( )",
		round(medians["call_mclapply"]/ medians["mclapply"], 2),
		round(medians["call_mclapply"]/ medians["map"], 2),
		round(medians["call_mclapply"]/ medians["lapply"], 2))
	
	mchof_data <- profile_mchof(len)[1,]
	control_data <- profile_controls(len)[1,]
	
	messagef(c(unlist(Map(
		function (name, multipier, speed) {
			sprintf("%s was %s times slower than the control (%s seconds)",
				name, multipier, speed)
		},
		names(mchof_data),
		round(mchof_data / control_data, 0),
		round(mchof_data, 0)))))
}

report_mchof_performance(1000, 100)
