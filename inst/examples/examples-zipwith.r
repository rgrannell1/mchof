
# get the mean of three vectors after zipping

mcZipWith(
	function (x) mean(unlist(x)),
	list(1:4, 2:5, 3:6)
)

# using mcZipWith to add names after zipping

mcZipWith(
	function (x) {
		list(name = x[[1]], id = x[[2]])
	},
	list(
		list('Jane', 'Jill', 'John'),
		list(1, 2, 3)
	),
	list(mc.cores = 2)
)

# or alternatively

mcZipWith(
	function (x) {
		structure(x, names = c('name', 'id'))
	},
	list(
		list('Jane', 'Jill', 'John'),
		list(1, 2, 3)
	),
	list(mc.cores = 2)
)

# add indices to a shuffled vector

mcZipWith (
	function (x) {
		list( x[[1]], ind = x[[2]] )
	}, 
	list(sample(letters[1:10]), 1:10)
)
