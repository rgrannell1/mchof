library(mchof)
library(multicore)
library(testthat)

# an assortment of generators for unit tests

bool_gen <- function(n=1, list = FALSE){
	# generates a list/vector of n random bools
	sample(c(TRUE, FALSE), size = n, replace = TRUE) 
}
number_gen <- function(n=100){
	runif(n, -100, 100)
}
string_gen <- function(n=100){
	replicate(n, 
		paste0(sample(letters[1:26], size = sample(5:10, size=1)), 
			collapse = ''))
}
paropts_gen <- function(n=1){
	sample(
		list(
			list(mc.preschedule=T),
			list(mc.silent = T, mc.cores = 2),
			list(mc.preschedule=F, mc.cores = 2)
		), size = n)	
}
string_fun <- function(n=1){
	sample(
		list(
			function(x)	grepl('a|b|c|d', x),
			function(x)	grepl('cat', x),
			function(x)	grepl('aa|bc', x)
		), size = n)
}
num_fun <- function(n=1){
	sample(
		list(
			function(x)	mean(x) > 2,
			function(x)	max(x) > 20,
			function(x)	min(x) < 101
	), size = n)
}
x_gen <- function(n=100, f){
	as.list(match.fun(f)(n))
}

test_package("mchof")
