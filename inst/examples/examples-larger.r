
# is the first function larger than the second when n = 10?

f <- function (n) 2*n
g <- function (n) 3*n - 4
first_bigger <- mcLarger(f, g)
first_bigger(10)

# select the values of x that make the first 
# function larger than the second

mcSelect(
	mcLarger(
		function (x) x^2 - 2,
		function (x) x^3 - 10),
	x = 1:10)



