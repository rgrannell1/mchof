
context("mcFlip: normal cases")

forall(info = "mcFlip reverses formal names",
	list(func_ = r_functions),
	function (func_) {
		
		rev_formals <- formals(mcFlip(func_))
		rev_formals_two <- rev(formals(func_))
		
		all(names(rev_formals) == names(rev_formals_two))
		
	}
)

forall(info = "mcFlip preserved and reverses formal values",
	list(func_ = r_functions),
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
	list(func_ = r_functions),
	function (func_) {
		
		permutation <- sample(seq_along(formals(func_)))
		
		rev_formals <- formals(mcJumble(func_, permutation))
		rev_formals_two <- formals(func_)[ c(permutation) ]
		
		all(names(rev_formals) == names(rev_formals_two))
		
	}
)

forall(info = "mcJumble preserved and reverses formal values",
	list(func_ = r_functions),
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

context("mcParameters: normal cases")

forall(info = "mcParameters can get the formals of non-primitive functions",
	list(func_ = r_functions),
	function (func_) {
		
		identical( formals(func_), mcParameters(func_) )

	},
	given = function (func_) {
		!is.primitive(func_) && is.function(func_)
	}
)

forall(info = "mcParameters can get the arguments of primitive functions",
	list(func_ = r_functions),
	function (func_) {
		
		arguments <- function (f) {
			head(as.list(args(f)), -1)
		}

		identical( arguments(func_), mcParameters(func_) )

	},
	given = function (func_) {
		is.primitive(func_)
	}
)

forall(info = "mcParameters can set the formals of non-primitive functions",
	list(func_ = r_functions),
	function (func_) {
		
		func_new <- mcParameters( func_, c("a", "b") )


	},
	given = function (func_) {
		!is.primitive(func_) && is.function(func_)
	}
)

forall(info = "mcParameters can set the arguments of primitive functions",
	list(func_ = r_functions),
	function (func_) {
		
		func_new <- mcParameters( func_, c("a", "b") )

	},
	given = function (func_) {
		is.primitive(func_)
	}
)