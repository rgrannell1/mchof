
# check that all the numbers in a list are odd

is_odd <- function (n) (n %% 2) == 0
mcOne(is_odd, c(2, 4, 6, 7, 8))

# check that every string is an email

is_email <- function (str) {
	grepl('.+@.+[.].+', str)
}

mcOne(
	is_email,
	c('me@gmail.com', 'you@yahoo.com', 'we@google.ie')	
)

# check that the mean value of every list is 
# larger than two

mcOne(
	function (x) mean(x) > 0,
	list(
		c(1,2,3),
		c(4,4,4),
		c(-10, 6)
))
