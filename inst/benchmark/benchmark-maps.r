
iterate_control <- function (x) {
	
	count <- 0
	while (count < max(x)) {
		count <- count + 1
	}
}

benchmark$maps <- 
	mcZipWith(
		mcExplode(
			function (x) list(test = x[[1]], control = x[[2]], name = x[[3]])
		),
		list(
			mcIterateWhile = function (x) {

				mcIterateWhile(
					function (n) n != max(x),
					function (n) n + 1,
					0)

			},
			mcIndMap = function (x) mcIndMap(function (x) NULL, x)
		),
		list(
			mcPosition = iterate_control,
			mcFind = function (x) lapply(x, function (x) NULL)
		),
		c("mcIterateWhile", "mcIndMap")
	)

