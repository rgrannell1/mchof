
# get the values of x
# for which one polynomial is larger than polynomial
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcSelect(
	mcLarger(
		function (x) x^2 - 2 * x + 3,
		function (x) 2^x^2 - 10
	),
	-10:10
)

# is composition of linear functions equivelant to 
# multiplication?
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

f <- function (x) x * 2
g <- function (x) x * 3

mcAll(
	mcEqual(
		f %of% g,
		function (x) x * 2 * 3
	),
	0:100
)

# partition a list into values for which
# sin + cos is bigger than sin + sin
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcPartition(
	mcLarger(
		mcPlus(sin, cos),
		mcPlus(sin, sin)),
	1:100
)

# find the intersection of two lines, if there
# is one
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcSelect(
	mcEqual(
		function (n) 2*n + 4,
		function (n) 3*n + 12
	),
	-100:100
)

# create the function sin(x) - cos(cos(x))
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcMinus(sin, cos %of% cos)

