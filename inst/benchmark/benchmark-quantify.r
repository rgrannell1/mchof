
quantifier_control <- function (x) {
	lapply(x, function (y) NULL)
}

true_func <- function (...) T
false_func <- Negate(true_func)

benchmark$quantify <- 
	mcZipWith(
		mcExplode(
			function (x) list(test = x[[1]], control = x[[2]], name = x[[3]])
		),
		list(
			mcAll = function (x) mcAll(true_func, x),
			mcAny = function (x) mcAny(false_func, x),
			mcOne = function (x) mcOne(false_func, x)
		),
		list(
			mcAll = quantifier_control,
			mcAny = quantifier_control,
			mcOne = quantifier_control
		),
		c("mcAll", "mcAny", "mcOne")
	)
