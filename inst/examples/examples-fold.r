
# bind a list of vectors into a column

mcFold(
	function (a, b) cbind(a, b),
	c(),
	list(
		1:4, 2:5,
		3:6, 4:7
))

# get the 4th power of a matrix 

fourth_power <- function (X) {
	I <- diag(1, nrow(X), ncol(X))
	
	mcFold(
		f = function (a, b) a %*% b,
		first = I,
		x = rep(list(X), 4))
}
fourth_power( t(matrix(1:4, 2, 2)) )


