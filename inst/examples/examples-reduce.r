
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
