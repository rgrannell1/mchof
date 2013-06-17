
zip_control <- function (x) {

	group_into(x, 2)

	lapply(x, null_func)
}

zip_control <- function (x) {
	Position(function(y) FALSE, x)
}

benchmark$zips <- 
	mcZipWith(
		mcExplode(
			function (x) list(test = x[[1]], control = x[[2]], name = x[[3]])
		),
		list(
			mcZipWith = function (x) {

				do.call(
					mcZipWith,
					c(list(f = function (y) NULL), group_into(x, 2)))
			},
			mcUnzipWith = function (x) {
				mcUnzipWith(function (y) NULL, x)
			}
		),
		list(
			mcPosition = zip_control,
			mcFind = zip_control
		),
		c("mcPosition", "mcFind")
	)