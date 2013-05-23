
# find the first value larger than 10

mcPosition(
	function (n) n > 10,
	c(1,2,11,3,4,-2))

# check for a match from the right

mcPosition(
	function (n) n > 10,
	c(1,2,11,3,4,-2),
	right = TRUE)

# find the first string matching a regular expression

mcPosition(
	function (n) grepl('yahoo', n),
	c('greg@gmail.com', 'bob@yahoo.com', 'phil@yahoo.ie')
)
