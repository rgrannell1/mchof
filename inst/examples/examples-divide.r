
# function is vectorised

f <- function (x) x^2
g <- function (x) x^3

square_over_cube <- mcDivide(f, g)
square_over_cube(1:10)

# invoking the function without intermediate steps

mcDivide(
	function (n) n + n,
	function (n) n^2) (1:5)
