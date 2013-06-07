
require(microbenchmark)
require(mchof)

one_func <- function (...) 1
true_func <- function (...) TRUE
false_func <- Negate(true_func)
null_func <- function (...) NULL

fold_control <- function (x) Reduce(null_func, x)

quantifier_control <- filter_control <- 
	zip_control <- position_control <- function (x) {
	parallel::mclapply(x, null_func)
}

mchof_tests <- mcZipWith(
	function (x) {
		list(test = x[[1]], control = x[[2]], name = x[[3]])	
	},
	list(
		function (x) mcAll(true_func, x),
		function (x) mcAny(false_func, x),
		function (x) mcFilter(true_func, x),
		function (x) mcFind(false_func, x),
		function (x) mcFold(one_func, 0, x),
		function (x) mcOne(false_func, x),
		function (x) mcPartition(true_func, x),
		function (x) mcPosition(false_func, x),
		function (x) mcReduce(one_func, x),
		function (x) mcReject(false_func, x),
		function (x) {
			zippable_x <- list(
				head(x, floor(length(x) / 2)),
				tail(x, ceiling(length(x) / 2)))
			
			do.call(mcZipWith, list(f=null_func, zippable_x))
		},
		function (x) mcUnzipWith(true_func, x)
	),
	list(
		function (x) quantifier_control(x),
		function (x) quantifier_control(x),
		function (x) filter_control(x),
		function (x) position_control(x),
		function (x) fold_control(x),
		function (x) quantifier_control(x),
		function (x) filter_control(x),
		function (x) position_control(x),
		function (x) fold_control(x),
		function (x) filter_control(x),
		function (x) zip_control(x),
		function (x) zip_control(x)	
	),
	c("mcAll", "mcAny", "mcFilter", "mcFind",
		"mcFold", "mcOne", "mcPartition", "mcPosition",
		"mcReduce", "mcReject", "mcZipWith", "mcUnzipWith")
)

backend_tests <- mcZipWith(
	function (x) {
		list(test = x[[1]], control = x[[2]], name = x[[3]])	
	},
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

options(mc.cores = NULL)

benchmark_code(backend_tests, len = 1000000, 10)
benchmark_code(mchof_tests, len = 1000000, 10)

