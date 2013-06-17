

fold_control <- function (x) {
	Reduce(function (a, b) 1, x)
}
one_func <- function (x, y) 1

benchmark$folds <- 
	mcZipWith(
		mcExplode(
			function (x) list(test = x[[1]], control = x[[2]], name = x[[3]])
		),
		list(
			mcFold = function (x) mcFold(one_func, first = 0, x = x),
			mcReduce =  function (x) mcReduce(one_func, x)
		),
		list(
			mcFold = fold_control,
			mcReduce = fold_control
		),
		c("mcFold", "mcReduce")
	)