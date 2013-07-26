

fold_control <- function (x) {
	Reduce(function (a, b) 1, x)
}
one_func <- function (x, y) 1

benchmark$folds <- 
	mcZipWith(
		explode(
			function (x) list(test = x[[1]], control = x[[2]], name = x[[3]])
		),
		list(
			mcFold = function (x) mcFold(one_func, 0, x),
			mcFoldl = function (x) mcFoldl(one_func, 0, x),
			mcFoldr = function (x) mcFoldr(one_func, 0, x),
			mcReduce =  function (x) mcReduce(one_func, x),
			mcReducel =  function (x) mcReducel(one_func, x),
			mcReducer =  function (x) mcReducer(one_func, x)
		),
		list(
			mcFold = fold_control,
			mcFoldl = fold_control,
			mcFoldr = fold_control,
			mcReduce = fold_control,
			mcReducel = fold_control,
			mcReducer = fold_control
		),
		c("mcFold", "mcFoldl", "mcFoldr",
		 "mcReduce", "mcReducel", "mcReducer")
	)