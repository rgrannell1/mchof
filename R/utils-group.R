
group_into <- function (x, size) {
	# groups x into chucks of size,
	# unless too few elements are left
	
	size <- abs(size)
	if (size == length(x)) {
		list(x)
	} else if (size == 0) {
		list()
	} else {	
		lapply(
			seq(from = 1, to = length(x), by = size),
			function (lower) {
				x[ lower:min(length(x), lower + size - 1) ]
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

