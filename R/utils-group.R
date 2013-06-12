
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

chop_into <- function (x, cuts) {
	cuts <- abs(cuts)
	
	if (cuts == 0) return (x)
	if (cuts > length(x)) return (x)
	group_into(x, floor(length(x) / cuts))
}

