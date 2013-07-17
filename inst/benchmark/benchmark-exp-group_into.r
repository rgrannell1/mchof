
# find the fastest way of grouping a vector into 
# groups of n

benchmark_exp$group_into <- ( function () {
	
	group_into_control <- function (x, size) {
		# groups x into chucks of size,
		# unless too few elements are left
		
		if (size == 1) {
			list(x)
		} else {	
			lapply(
				seq(from = 1, to = length(x), by = size),
				function (lower) {
					x[ lower:min(length(x), lower + size - 1) ]
			})
		}
	}
	group_into_len <- function (x, size) {
		# groups x into chucks of size,
		# unless too few elements are left
		
		if (size == 1) {
			list(x)
		} else {
			len_x <- length(x)
			lapply(
				seq(from = 1, to = len_x, by = size),
				function (lower) {
					x[ lower:min(len_x, lower + size - 1) ]
			})
		}
	}
	preallocated_group_into <- function (x, size) {
		if (size == 1) return (list(x))
		
		ind <- lower <- 1
		upper <- size
		len_x <- length(x)
		groups <- vector(mode = "list", length = ceiling(len_x / size))
		
		while (ind <= length(groups)) {
			groups[[ind]] <- x[
				((ind - 1) * size + 1) :
				(min( ((ind - 1) * size + 1) + size - 1,len_x)) ]
			ind <- ind + 1
		}
		groups
	}
	
	mcZipWith(
		explode(function (x) {
			list(test = x[[1]], control = x[[2]], name = x[[3]])	
		}),
		list(
			preallocated_loop = function (x) {
				preallocated_group_into(x, 2)
			},
			group_into_len = function (x) {
				group_into_len(x, 2)
			}),
		list(
			preallocated_loop = function (x) {
				group_into_control(x, 2)
		},
		group_into_len = function (x) {
			group_into_len(x, 2)
		}),
		c("preallocated_loop", "group_into_len")
	)
	
})()
