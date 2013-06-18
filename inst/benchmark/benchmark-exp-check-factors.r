
# throw errors if a any element in a list is a factor,
# return which values are NULL

benchmark_exp$check_ziplists <- ( function () {

	control_factor <- function (x) {
		sapply(x, function (y) NULL)
	}

	factor_1 <- function (x) {
		sapply(x, function (y) {

			if (inherits(y, "factor")) NULL

			is.null(y)
		})
	}
	factor_2 <- function (x) {
		sapply(x, function (y) {

			if (is.factor(y)) NULL

			is.null(y)
		})
	}

	factor_3 <- function (x) {
		lapply(x, function (y) {
			
			if (is.factor(y)) NULL

			is.null(y)
		})
	}
	factor_4 <- function (x) {
		lapply(x, function (y) {

			if (inherits(y, "factor"))

			is.null(y)
		})
	}

	factor_5 <- function (x) {
		vapply(x, function (y) {

			if (is.factor(y)) NULL

			is.null(y)
		}, c(1))
	}
	factor_6 <- function (x) {
		vapply(x, function (y) {

			if (inherits(y, "factor")) NULL

			is.null(y)
		}, c(1))
	}

	mcZipWith(
		mcExplode(function (x) {
			list(test = x[[1]], control = x[[2]], name = x[[3]])	
		}),
		list(
			factor_1, factor_2,
			factor_3, factor_4,
			factor_5, factor_6
		),
		list(
			control_factor, control_factor, control_factor,
			control_factor, control_factor, control_factor
		),
		c("factor_1", "factor_2", "factor_3",
			"factor_4", "factor_5", "factor_6")
	)

} )()
