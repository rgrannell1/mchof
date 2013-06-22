
context("mcFlip: normal cases")

arguments <- function (f) {
	head(as.list(args(f)), -1)
}

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
		

		identical( arguments(func_), mcParameters(func_) )

	},
	given = function (func_) {
		is.primitive(func_)
	}
)		
forall(info = "mcParameters can set the formals of non-primitive functions",
	list(func_ = r_functions, name_ = r_words()),
	function (func_, name_) {
		
		func_new <- mcParameters( func_, name_ )
		names(formals(func_new)) == name_

	},
	given = function (func_, name_) {
		!is.primitive(func_) && is.function(func_)
	}
)

forall(info = "mcParameters can set the arguments of primitive functions",
	list(func_ = r_functions, name_ = r_words()),
	function (func_, name_) {
		
		func_new <- mcParameters( func_, name_)
		names(arguments(func_new)) == name_

	},
	given = function (func_, name_) {
		is.primitive(func_)
	}
)

context("mcExplode and mcImplode: normal cases")


context("mcApply: normal cases")

forall(info = "mcApply acts like do.call when called with a list",
	list(x_ = r_seq_len()),
	function (x_) {

		sumlist <- function (x) sum( unlist(x) )

		x_ <- as.list(x_)
		( sumlist %apply% list(list(x_)) == do.call(sum, x_) ) &&
		( sumlist %apply% "x_" == do.call(sum, x_) )
	}
)




