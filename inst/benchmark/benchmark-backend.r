
null_func <- function (x) NULL

benchmark$backend <- mcZipWith(
	mcExplode(function (x) {
		list(test = x[[1]], control = x[[2]], name = x[[3]])	
	}),
	list(
		function (x) parallel::mclapply(x, null_func),	
		function (x) Map(null_func, x),
		function (x) lapply(x, null_func)
	),
	list(
		function (x) call_mclapply(null_func, x),
		function (x) call_mclapply(null_func, x),
		function (x) call_mclapply(null_func, x)
	),
	c("mclapply", "map", "lapply")
)

benchmark$dual_core <- mcZipWith(
	mcExplode(function (x) {
		list(test = x[[1]], control = x[[2]], name = x[[3]])	
	}),
	list(
		function (x) parallel::mclapply(x, null_func),	
		function (x) Map(null_func, x),
		function (x) lapply(x, null_func)
	),
	list(
		function (x) call_mclapply(null_func, x, list(mc.cores = 2)),
		function (x) call_mclapply(null_func, x, list(mc.cores = 2)),
		function (x) call_mclapply(null_func, x, list(mc.cores = 2))
	),
	c("mclapply", "map", "lapply")
)
