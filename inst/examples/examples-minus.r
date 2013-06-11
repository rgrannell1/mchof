
# function is vectorised

f <- function (x) x^2
g <- function (x) x^3

square_minus_cube <- mcMinus(f, g)
square_minus_cube(1:10)

# invoking the function without intermediate steps

mcMinus(
	function (n) n + n,
	function (n) n^2) (1:5)


