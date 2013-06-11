
# function is vectorised

f <- function (x) x^2
g <- function (x) x^3

square_plus_cube <- mcPlus(f, g)
square_plus_cube(1:10)

# invoking the function without intermediate steps

mcPlus(
	function (n) n + n,
	function (n) n^2) (1:5)

