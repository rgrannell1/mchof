
benchmark$maps <- 
	mcZipWith(
		explode(
			function (x) list(test = x[[1]], control = x[[2]], name = x[[3]])
		),
		list(
			mcIndMap = function (x) mcIndMap(function (x, y) NULL, x)
		),
		list(
			mcIndMap = function (x) {				
				Map(
					function (x, y) NULL,
					x,
					seq_along(x))
			}
		),
		c("mcIndMap")
	)
