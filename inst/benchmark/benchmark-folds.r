

fold_control <- function (x) {
	Reduce(null_func, x)
}
one_func <- function (...) 1

benchmark$folds <- 
	mcZipWith(
		mcExplode(
			function (x) list(test = x[[1]], control = x[[2]], name = x[[3]])
		),
		list(
			mcFold = function (x) mcFold(one_func, 0, x),
			mcReduce =  function (x) mcReduce(one_func, x),
		),
		list(
			mcFold = fold_control,
			mcReduce = fold_control
		),
		c("mcFold", "mcReduce")
	)