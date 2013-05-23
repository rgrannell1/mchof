
# check that all the numbers in a list are odd

is_odd <- function (n) (n %% 2) == 0
mcAny(is_odd, c(2, 4, 6, 7, 8))

# check that every string is an email

is_email <- function (str) {
	grepl('.+@.+[.].+', str)
}

mcAny(
	is_email,
	c('me@gmail.com', 'you@yahoo.com', 'we@google.ie')	
)

# check that the mean value of every list is 
# larger than two

mcAny(
	function (x) mean(x) > 0,
	list(
		1:3,
		6:2,
		-2:6
))
