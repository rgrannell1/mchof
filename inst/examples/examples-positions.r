
# find the first value larger than 10

mcPosition(
	function (n) n > 10,
	c(1,2,11,3,4,-2))

# find the first string matching a regular expression

mcPosition(
	function (n) grepl('yahoo', n),
	c('greg@gmail.com', 'bob@yahoo.com', 'phil@yahoo.ie')
)

# return the first value larger than 10

mcFind(
	function (n) n > 10,
	c(1,2,11,3,4,-2))

# return the first string matching a regular expression

mcFind(
	function (n) grepl('yahoo', n),
	c('greg@gmail.com', 'bob@yahoo.com', 'phil@yahoo.ie')
)

