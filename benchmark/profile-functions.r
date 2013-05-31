
library (ggplot2)
library (reshape2)
library (microbenchmark)

test_all_functions <- function (n = 5) {
	# return time data for every function in mchof,
	# for one value of n
	
	run_test <- function (which, n) {
		
		stopifnot(n > 1)
		
		ziplist <- list(
			seq_len(floor(n/2)),
			( ceiling(floor(n/2)) + 1):n)
		
		switch (which, 
			"mcAll" = mcAll(function (y) TRUE, seq_len(n)),
			"mcAny" = mcAny(function (y) FALSE, seq_len(n)),
			"mcFilter" = mcFilter(function (y) TRUE, seq_len(n)),
			"mcFind" = mcFind(function (y) FALSE, seq_len(n)),
			"mcFold" = mcFold(function (...) 1, 0, seq_len(n)),
			"mcOne" = mcOne(function (y) FALSE, seq_len(n)),
			"mcPartition" = mcPartition(function (y) TRUE, seq_len(n)),
			"mcPosition" = mcPosition(function (y) FALSE, seq_len(n)),
			"mcReduce" = mcReduce(function (...) 1, seq_len(n)),
			"mcReject" = mcReject(function (y) FALSE, seq_len(n)),
			"mcZipWith" = mcZipWith(function (y) TRUE, ziplist),
			"mcUnzipWith" = mcUnzipWith(function (y) TRUE, ziplist)
		)
	}
	
	list(
		times = summary(microbenchmark(
			run_test("mcAll", n), run_test("mcAny", n),
			run_test("mcFilter", n), run_test("mcFind", n),
			run_test("mcFold", n), run_test("mcOne", n),
			run_test("mcPartition", n), run_test("mcPosition", n),
			run_test("mcReduce", n), run_test("mcReject", n),
			run_test("mcUnzip", n), run_test("mcUnzipWith", n),
			run_test("mcZip", n), run_test("mcZipWith", n),
			times = 2
		))	
	)
}
