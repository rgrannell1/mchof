
# function is vectorised

f <- function (x) x^2
g <- function (x) x^3

square_by_cube <- mcMultiply(f, g)
square_by_cube(1:10)

# invoking the function without intermediate steps

mcMultiply(
	function (n) n + n,
	function (n) n^2) (1:5)


