
filter_control <- function (x) {
	lapply(x, function (y) NULL)
}

benchmark$filters <- 
	mcZipWith(
		mcExplode(
			function (x) list(test = x[[1]], control = x[[2]], name = x[[3]])
		),
		list(
			mcFilter = function (x) mcFilter(true_func, x),
			mcReject = function (x) mcReject(false_func, x),
			mcPartition = function (x) mcPartition(true_func, x)
		),
		list(
			mcFilter = filter_control,
			mcReject = filter_control, 
			mcPartition = filter_control
		),
		c("mcFilter", "mcReject", "mcPartition")
	)