
squash <- function (f) {
	function (...) f(list(...))
}

# unzip three lists & convert them to two string vectors

mcUnzipWith(
	squash(paste0),
	list(
		list('Jane', 1),
		list('Jill', 2),
		list('Joan', 3)
))

# get the mean of each 'column' of lists

mcUnzipWith(
	squash( function (x) mean(unlist(x)) ),
	list(
		list(0.2, 0.10),
		list(0.5, 0.02),
		list(12.2, 0.2)
))

# name the unzipped output

mcUnzipWith(
	squash( function (x) {
		list(ind_1 = x[1], ind_2 = x[2], ind_3 = x[3])
	} ),
	list(
		list('FORTRAN', 'no'),
		list('HASKELL', 'yes'),
		list('CLOJURE', 'yes')
))
