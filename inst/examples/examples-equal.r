
f <- function (x) 2
g <- function (x) 2 * x

f_equals_g <- mcEqual(f, g)
f_equals_g(1)

# invoking the function without intermediate steps

mcEqual(
	function (n) n + n,
	function (n) n^2) (1)


