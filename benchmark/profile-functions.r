
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
	
	timing_map <- microbenchmark(
		Map(identity, seq_len(n)),
		times = times
	)$time
	timing_mclapply <- microbenchmark(
		parallel::mclapply(seq_len(n), identity),
		times = times
	)$time
	timing_call_mclapply <- microbenchmark(
		call_mclapply(identity, seq_len(n)),
		times = times
	)$time
	
	medians <- c(
		map = median(timing_map),
		mclapply = median(timing_mclapply),
		call_mclapply = median(timing_call_mclapply))
	messagef(
		"call_mclapply was %s times slower than mclapply( ) and 
		%s times slower than Map( )",
		round(medians["call_mclapply"]/ medians["mclapply"], 3),
		round(medians["call_mclapply"]/ medians["map"], 3))
	
	cat("\n\n\n")
	
	mchof_data <- profile_mchof(n)[1,]
	control_data <- profile_controls(n)[1,]
	
	messagef(c(unlist(Map(
		function (name, multipier, speed) {
			sprintf("%s was %s times slower than the control (%s seconds)",
				name, multipier, speed
			)
		},
		names(mchof_data),
		round(mchof_data / control_data, 0),
		round(mchof_data, 0))))
	)
	
	profile_backend()
}

profile_backend <- function (times = 10) {
	# plot the speed of the backend
	
	Map(
		function (n) {
			
		},
		10^(1:7))
}

report_mchof_performance(10000, 1)






