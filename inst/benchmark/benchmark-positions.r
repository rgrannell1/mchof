
position_control <- function (x) {
	Position(function(y) FALSE, x)
}

benchmark$positions <- 
	mcZipWith(
		mcExplode(
			function (x) list(test = x[[1]], control = x[[2]], name = x[[3]])
		),
		list(
			mcPosition = function (x) mcPosition(function(y) FALSE, x),
			mcFind =  function (x) mcFind(function(y) FALSE, x)
		),
		list(
			mcPosition = position_control,
			mcFind = position_control
		),
		c("mcPosition", "mcFind")
	)