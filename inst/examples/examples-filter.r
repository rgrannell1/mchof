# remove NA values from a vector 

p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2))

# the same example, in parallel
p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2, list(mc.cores = 2)))

# find all even numbers in a vector of numbers
even_ints <- function(x){
	Filter(
		f = function(y) !(y %% 2),
		x
	)
}
even_ints(c(1,2,3,4,5,6,7,8,9,10))

# a more advanced example, using an anonymous function to
# filter out combinations that don't meet a predicate 

mcFilter(
	f = function(pair){
		val <- sum(unlist(pair))
		val > 8
	}, 
	x = apply(combn(8, 3), 2, list),
	paropts = list(mc.cores = 2)
)

# remove NULL values from a list, by first transforming the
# is.null function to a !is.null function

mcFilter(
	Negate(is.null),
	list(NULL, 1, 2, 3, NULL, 4, 5)
)
