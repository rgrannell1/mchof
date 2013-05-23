# remove NA values from a vector 
p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2))

# the same example, in parallel
p <- function(x) !is.na(x)
mcFilter(p, c(3,2,6,NA, 2, list(mc.cores = 2)))

# find all even numbers in a vector of numbers
even_ints <- function(x){
    Filter(
        f = function(y) if(is.integer(y) && !(y %% 2)) TRUE else FALSE,
	       x)
}
even_ints(c(1L,2L,3L,4L,5L,6L,7L,8L,9L,10L))

# a more advanced example, using anonymous functions to
# filter out combinations that don't meet a predicate 

mcFilter(
    f = function(pair){
        val <- sum(unlist(pair))
 	   if(val > 8) TRUE else FALSE
    }, 
    x = apply(combn(8, 3), 2, list),
    paropts = list(mc.cores = 2))
