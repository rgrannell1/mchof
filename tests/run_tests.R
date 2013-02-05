library(mchof)
library(multicore)
library(testthat)

bool_gen <- function(n=1, list = FALSE){
	# generates a list/vector of n random bools
	sample(c(TRUE, FALSE), size = n, replace = TRUE) 
}
number_gen <- function(n=100){
	
}
x_gen <- function(){
	
	
}

test_package("mchof")