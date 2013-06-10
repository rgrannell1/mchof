
require(microbenchmark)
require(mchof)

# NOTE: these tests are profiling the worst-case behaviour of
# these functions. Most functions have faster average case behaviour

one_func <- function (...) 1
true_func <- function (...) TRUE
false_func <- Negate(true_func)
null_func <- function (...) NULL

fold_control <- function (x) {
	Reduce(null_func, x)
}

quantifier_control <- filter_control <- function (x) {
	lapply(x, null_func)
}
zip_control <- function (x) {	
	lapply(x, null_func)
}
position_control <- function (x) {
	Position(function(y) FALSE, x)
}

mchof_tests <- mcZipWith(
	squash(function (x) {
		list(test = x[[1]], control = x[[2]], name = x[[3]])	
	}),
	list(
		mcAll = function (x) mcAll(true_func, x),
		mcAny = function (x) mcAny(false_func, x),
		mcFilter = function (x) mcFilter(true_func, x),
		mcFind = function (x) mcFind(false_func, x),
		mcFold = function (x) mcFold(one_func, 0, x),
		mcOne = function (x) mcOne(false_func, x),
		mcPartition = function (x) mcPartition(true_func, x),
		mcPosition = function (x) mcPosition(false_func, x),
		mcReduce = function (x) mcReduce(one_func, x),
		mcReject = function (x) mcReject(false_func, x),
		mcZipWith = function (x) {

			do.call(
				mcZipWith,
				c(list(f = null_func), group_into(x, 2)))
		},
		mcUnzipWith = function (x) mcUnzipWith(null_func, x)
	),
	list(
		mcAll = function (x) quantifier_control(x),
		mcAny = function (x) quantifier_control(x),
		mcFilter = function (x) filter_control(x),
		mcFind = function (x) position_control(x),
		mcFold = function (x) fold_control(x),
		mcOne = function (x) quantifier_control(x),
		mcPartition = function (x) filter_control(x),
		mcPosition = function (x) position_control(x),
		mcReduce = function (x) fold_control(x),
		mcReject = function (x) filter_control(x),
		mcZipWith = function (x) zip_control(x),
		mcUnzipWith = function (x) zip_control(x)	
	),
	c("mcAll", "mcAny", "mcFilter", "mcFind",
		"mcFold", "mcOne", "mcPartition", "mcPosition",
		"mcReduce", "mcReject", "mcZipWith", "mcUnzipWith")
)

backend_tests <- mcZipWith(
	squash(function (x) {
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

dual_core_backend_tests <- mcZipWith(
	squash(function (x) {
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
