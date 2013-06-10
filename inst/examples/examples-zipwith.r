

# a helper function that takes a function that 
# takes on variable and makes it work with ...

squash <- function (f) {
	function (...) f(list(...))
}

# get the mean of three vectors after zipping

# get the list [ [1*1] [2*2] ... [10 * 10] ]

mcZipWith ('*', 1:10, 1:10)

mcZipWith(
	function (...) mean(unlist(...)),
	1:4, 2:5, 3:6
)

# using mcZipWith to add names after zipping

mcZipWith(
	squash(function (x) {
		list(name = x[[1]], id = x[[2]])
	}),
	list('Jane', 'Jill', 'John'),
	list(1, 2, 3),
	paropts = list(mc.cores = 2)
)

# or alternatively

mcZipWith(
	squash(function (x) {
		structure(x, names = c('name', 'id'))
	}),
	list('Jane', 'Jill', 'John'),
	list(1, 2, 3),
	paropts = list(mc.cores = 2)
)

# add indices to a shuffled vector

mcZipWith (
	squash(function (x) {
		list( x[[1]], ind = x[[2]] )
	}), 
	sample(letters[1:10]),
	1:10
)
