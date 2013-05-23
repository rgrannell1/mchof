
# return the first value larger than 10

mcFind(
	function (n) n > 10,
	c(1,2,11,3,4,-2))

# return the first match from the right

mcFind(
	function (n) n > 10,
	c(1,2,11,3,4,-2),
	right = TRUE)

# return the first string matching a regular expression

mcFind(
	function (n) grepl('yahoo', n),
	c('greg@gmail.com', 'bob@yahoo.com', 'phil@yahoo.ie')
)
