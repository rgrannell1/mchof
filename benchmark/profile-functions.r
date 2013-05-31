
library (microbenchmark)

profile_mchof <- function (len = 10, times = 1) {
	# return time data for every function in mchof,
	# for one value of n
	
	run_test <- function (which, len) {
		stopifnot(len > 1)
		
		ziplist <- list(
			seq_len(floor(len / 2)),
			(ceiling(floor(len / 2)) + 1):len)
		
		switch (which, 
			"mcAll" = mcAll(function (y) TRUE, seq_len()),
			"mcAny" = mcAny(function (y) FALSE, seq_len(len)),
			"mcFilter" = mcFilter(function (y) TRUE, seq_len(len)),
			"mcFind" = mcFind(function (y) FALSE, seq_len(len)),
			"mcFold" = mcFold(function (...) 1, 0, seq_len(len)),
			"mcOne" = mcOne(function (y) FALSE, seq_len(len)),
			"mcPartition" = mcPartition(function (y) TRUE, seq_len(len)),
			"mcPosition" = mcPosition(function (y) FALSE, seq_len(len)),
			"mcReduce" = mcReduce(function (...) 1, seq_len(len)),
			"mcReject" = mcReject(function (y) FALSE, seq_len(len)),
			"mcZipWith" = mcZipWith(function (y) TRUE, ziplist),
			"mcUnzipWith" = mcUnzipWith(function (y) TRUE, ziplist)
		)
	}
	
	cbind(
		summary(microbenchmark(
		run_test("mcAll", len), run_test("mcAny", len),
		run_test("mcFilter", len), run_test("mcFind", len),
		run_test("mcFold", len), run_test("mcOne", len),
		run_test("mcPartition", len), run_test("mcPosition", len),
		run_test("mcReduce", len), run_test("mcReject", len),
		run_test("mcUnzip", len), run_test("mcUnzipWith", len),
		run_test("mcZip", len), run_test("mcZipWith", len),
		times = times)), data_length = len, date = date(),
		funcs = sapply(
			list(
				mcAll, mcAny, mcFilter, mcFind, mcFold, mcOne, 
				mcPartition, mcPosition, mcReduce, mcReject, 
				mcUnzip, mcUnzipWith, mcZip, mcZipWith),
				function (f) {
					paste0(deparse(f), collapse = "\n")
				}
	))
}
