
# a helper function that takes a function that 
# takes on variable and makes it work with ...

mcExplode <- function (f) {
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
	mcExplode(function (x) {
		list(name = x[[1]], id = x[[2]])
	}),
	list('Jane', 'Jill', 'John'),
	list(1, 2, 3),
	paropts = list(mc.cores = 2)
)

# or alternatively

mcZipWith(
	mcExplode(function (x) {
		structure(x, names = c('name', 'id'))
	}),
	list('Jane', 'Jill', 'John'),
	list(1, 2, 3),
	paropts = list(mc.cores = 2)
)

# add indices to a shuffled vector

mcZipWith (
	mcExplode(function (x) {
		list( x[[1]], ind = x[[2]] )
	}), 
	sample(letters[1:10]),
	1:10
)


# zip two vectors into a list of 2-element lists

mcZip(1:5, letters[1:5])

# zip three lists togerther

mcZip(
	list('R', 'Matlab', 'SAS'),
	list('language', 'language', 'languge'),
	list('GNU', 'not GNU', 'not GNU')
)

# zip two lists, with some elements discarded

mcZip(1:10,letters[1:4])


mcExplode <- function (f) {
	function (...) f(list(...))
}

# unzip three lists & convert them to two string vectors

mcUnzipWith(
	mcExplode(paste0),
	list(
		list('Jane', 1),
		list('Jill', 2),
		list('Joan', 3)
))

# get the mean of each 'column' of lists

mcUnzipWith(
	mcExplode( function (x) mean(unlist(x)) ),
	list(
		list(0.2, 0.10),
		list(0.5, 0.02),
		list(12.2, 0.2)
))

# name the unzipped output

mcUnzipWith(
	mcExplode( function (x) {
		list(ind_1 = x[1], ind_2 = x[2], ind_3 = x[3])
	} ),
	list(
		list('FORTRAN', 'no'),
		list('HASKELL', 'yes'),
		list('CLOJURE', 'yes')
))


# unzip three lists of 2-elements into two 3-element lists

mcUnzip(
	list(
		c('hat', 'dance'),
		c('may', 'pole'),
		c('silicon', 'chip')
))
