
group_into <- function (xs, n) {

	pcall <- sys.call()	
	require_a('listy', xs, pcall)
	require_a('nonnegative whole', n, pcall)

	if (length(xs) == 0) {
		list()
	} else {
		lapply(
			seq(from = 1, to = length(xs), by = n),
			function (lower) {
				xs[ lower:min(length(xs), lower + n - 1) ]
		})
	}
}

chop_into <- function (x, pieces) {
	# chop a vector x into pieces...pieces,
	# if it's possible
	
	pieces <- abs(pieces)
	
	if (pieces > length(x)) return (group_into(x, 1))
	
	average_size <- ceiling(length(x) / pieces)
	
	lapply(
		seq(from = 1, to = length(x), length.out = pieces),
		function (lower) {
			x[ lower:min(length(x), lower + average_size) ]
	})
}

