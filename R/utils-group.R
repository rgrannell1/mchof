
group_into <- function (X, n) {

	pcall <- sys.call()	
	require_a('listy', X, pcall)
	require_a('positive whole', n, pcall)

	if (length(X) == 0) {
		list()
	} else {
		lapply(
			seq(from = 1, to = length(x), by = n),
			function (lower) {
				x[ lower:min(length(x), lower + n - 1) ]
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

