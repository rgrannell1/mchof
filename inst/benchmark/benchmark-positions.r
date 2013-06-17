
position_control <- function (x) {
	Position(function(...) FALSE, x)
}

benchmark$positions <- 
	mcZipWith(
		mcExplode(
			function (x) list(test = x[[1]], control = x[[2]], name = x[[3]])
		),
		list(
			mcPosition = function (x) mcPosition(function(...) FALSE, x = x),
			mcFind =  function (x) mcFind(function(...) FALSE, x = x)
		),
		list(
			mcPosition = position_control,
			mcFind = position_control
		),
		c("mcPosition", "mcFind")
	)
