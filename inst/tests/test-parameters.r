
context("mcFlip: normal cases")

forall(info = "mcFlip reverses formal names",
	list(func_ = list(Reduce, Position, Find, mcFind, mcReduce, mcFold, mcZip)),
	function (func_) {
		
		rev_formals <- formals(mcFlip(func_))
		rev_formals_two <- rev(formals(func_))
		
		all(names(rev_formals) == names(rev_formals_two))
		
	}
)

forall(info = "mcFlip preserved and reverses formal values",
	list(func_ = list(Reduce, Position, Find, mcFind, mcReduce, mcFold, mcZip)),
	function (func_) {
		
		rev_formals <- formals(mcFlip(func_))
		rev_formals_two <- rev(formals(func_))
		
		all(unlist(
			Map(
				function(e1, e2) {
					identical(e1, e2)
				},
				rev_formals,
				rev_formals_two)))
	}
)

context("mcJumble: normal cases")

forall(info = "mcJumble permutes formal names",
	list(func_ = list(Reduce, Position, Find, mcFind, mcReduce, mcFold, mcZip)),
	function (func_) {
		
		permutation <- sample(seq_along(formals(func_)))
		
		rev_formals <- formals(mcJumble(func_, permutation))
		rev_formals_two <- formals(func_)[ c(permutation) ]
		
		all(names(rev_formals) == names(rev_formals_two))
		
	}
)

forall(info = "mcJumble preserved and reverses formal values",
	list(func_ = list(Reduce, Position, Find, mcFind, mcReduce, mcFold, mcZip)),
	function (func_) {
		
		permutation <- sample(seq_along(formals(func_)))
		
		rev_formals <- formals(mcJumble(func_, permutation))
		rev_formals_two <- formals(func_)[ c(permutation) ]
			
		all(unlist(
			Map(
				function(e1, e2) {
					identical(e1, e2)
				},
				rev_formals,
				rev_formals_two)))	
	}
)

