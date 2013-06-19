
# find all sexy primes below 1000

mcFold













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


# reduce integer operations over a list

mcReduce('+', 1:10)
mcReduce('*', 1:6)

# find the maximum value in a vector

mcReduce(max, sample(1:10))

# find the maximum value in a list, with 
# a custom max function

max_matrix <- function (A, B) {

	size_one <- prod(dim(A)) * prod(A)
	size_two <- prod(dim(B)) * prod(B)
	
	if (size_one > size_two) A else B 
}

mcReduce(
	max_matrix,
	list(
		matrix(1:6, 2, 3),
		matrix(1:4, 2, 2),
		matrix(1:16,4, 4)
))
