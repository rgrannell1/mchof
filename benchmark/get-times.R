
library (ggplot2)
library (reshape2)

run_test <- function (expr) system.time(expr)[3]	

test_reduce <- function (x) {
	mcReduce(function (a, b) 1, x)
}
test_old_reduce <- function (x) {
	Reduce(function (a, b) 1, x)
}
test_fold <- function (x) {
	mcFold(function (a, b) 1, 0, x)
}
test_zip <- function (x) {
	mcZip(
		list(
			head(x, floor(length(x)/2)),
			tail(x, ceiling(length(x)/2)))
	)
}
test_unzip <- function (x) {
	mcUnzip(
		list(
			head(x, floor(length(x)/2)),
			tail(x, ceiling(length(x)/2)))
	)
}
test_zipwith <- function (x) {
	mcZipWith(paste0, x)
}
test_unzipwith <- function (x) {
	mcUnzipWith(paste0, x)
}
test_filter <- function (x) {
	mcFilter(function (y) FALSE, x)
}
test_partition <- function (x) {
	mcPartition(function (y) FALSE, x)
}
test_reject <- function (x) {
	mcReject(function (y) FALSE, x)
}
test_all <- function (x) {
	mcAll(function (y) TRUE, x)
}
test_any <- function (x) {
	mcAny(function (y) TRUE, x)
}
test_one <- function (x) {
	mcOne(function (y) TRUE, x)
}
test_position <- function (x) {
	mcPosition(function (y) FALSE, x)
}
test_find <- function (x) {
	mcFind(function (y) FALSE, x)
}

timing <- Map(
	function (n) {
		x <- rep(1, n)
		print(n)
		
		c(
			reduce = run_test(test_reduce(x)),
			fold = run_test(test_fold(x)),		
			old_reduce = run_test(test_old_reduce(x)),
			zip = run_test(test_zip(x)),
			unzip = run_test(test_unzip(x)),
			zipwith = run_test(test_zipwith(x)),
			unzipwith = run_test(test_unzipwith(x)),
			filter = run_test(test_filter(x)),
			partition = run_test(test_partition(x)),
			reject = run_test(test_reject(x)),
			all = run_test(test_all(x)),
			any = run_test(test_any(x)),
			one = run_test(test_one(x)),
			position = run_test(test_position(x)),
			find = run_test(test_find(x)))
	},
	round(seq(1, 100000, len = 6), 0)
)

func_times <- structure(
	Map(unlist, mcUnzip(timing)),
	names =  c(
		"reduce", "fold", "old_reduce", "zip", "unzip", "zipwith", "unzipwith",
		"filter", "partition", "reject", "all", "any", "one", "position",
		"find")
)
dframe_func_times <- cbind(ind = 1:6, melt(as.data.frame(func_times)))

ggplot(data = dframe_func_times) + 
geom_line(aes(x = ind, y = value, color = variable)) +
xlab("a x n") + ylab("seconds") + ggtitle("mchof benchmarks")



