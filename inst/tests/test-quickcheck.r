
context('quickcheck ')

test_that('', {
	
	rand_paropts <- list(
		list(mc.cores = 1), list(mc.cores = 2), 
		list(mc.cores = 3), list(mc.cores = 4)	
	)
	rand_lists_with_null <- function () {
		# generate random lists, maybe with some null elements	
		
		list_size <- sample(1:100, size = 1)
		sample(
			list(NULL, 101, 312.2, 'cat', matrix(0,2,2)),
			size = list_size, replace = TRUE)
		
	}
	all_equal <- function (list) {
		# are all elements of a list identical?
		length(which(!duplicated(list))) == 1
	}
	
	forall(
		list(
			x_ = rand_lists_with_null(),
			paropts_ = rand_paropts),
		function (x_, paropts_) {
			
			to_exclude <- sapply(x_, is.null)
			
			NULL_free <- if (length(to_exclude) > 0) x_[-to_exclude] else x_ 
			
			results <- list(
				mcFilter(Negate(is.null), x_, paropts_),
				mcReject(is.null, x_, paropts_),
				mcPartition(is.null, x_, paropts_) [[2]],
				mcPartition(Negate(is.null), x_, paropts_) [[1]],
				NULL_free
			)
			
			all_equal(results)
		}
	)
	

})
