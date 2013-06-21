
# mcZip is a very basic function;
# it just turns n-lists into a list of n-element lists
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcZip(1:10, 11:20, 21:30)

# mcUnzip operates on a single list,
# rather than a variable number of lists
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcUnzip( list(1:10, 11:20, 21:30) )

# mcZipWith can be used for creating 
# vectorised n-ary functions.
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

add3 <- function (x, y, z) mcZipWith(
	function (a, b, c) a + b + c,
	x, y, z
)
add3(1:3, 11:13, 3:1)

