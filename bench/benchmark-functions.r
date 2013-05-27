
library(microbenchmark)
library(ggplot2)

# benchmark the performance of functions in the current version of 
# mchof

format_timings <- function (time_series) {
	c(median = time_series$median, interquartile = 
	  	time_series$uq - time_series$lq)
}

reduce_test <- function (x) mcReduce('+', x)
fold_test <- function (x) mcFold('+', 0, x)

benchmark_results <- lapply(
	X = seq_len(10),
	FUN = function (n) {
		
		x <- seq_len(n)
		
		data.frame(
			reduce = format_timings( summary(microbenchmark( reduce_test(x) ))),
			fold = format_timings( summary(microbenchmark( fold_test(x) )) ))
})
mcFold(
	function (acc, new) {
		rbind(new, acc)
	}, NULL, benchmark_results
)











