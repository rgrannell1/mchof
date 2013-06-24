
context("mcFlip: normal cases")

register$add(
	"mcFlip", type = "test",
	version = "0.4", is_finished = FALSE
)

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

register$add(
	"mcJumble", type = "test",
	version = "0.4", is_finished = FALSE
)

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

register$add(
	"mcParameters", type = "test",
	version = "0.4", is_finished = FALSE
)

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

context("mcExplode and mcImplode: normal cases")

register$add(
	c("mcExplode", "mcImplode") type = "test",
	version = "0.4", is_finished = FALSE
)

forall(info = "mcExplode returns a variadic function",
	list(x_ = r_seq_len()),
	function (x_) {

		res <- mcZipWith(
			mcExplode(
				function (li) {
					li[[1]] == li[[2]]
				}
			),
			x_, x_
		)
		all(unlist(res))

	}
)

forall(info = "mcImplode returns a single variable function",
	list(x_ = r_seq_len()),
	function (x_) {

		res <- mcFilter(
			mcImplode( function (a, b) a == b ),
			mcZip(x_, x_)
		)

		identical(res, mcZip(x_, x_))
	}
)

context("mcApply: normal cases")

register$add(
	"mcApply", type = "test",
	version = "0.4", is_finished = FALSE
)

forall(info = "mcApply works with an args list, or a name vector",
	list(x_ = r_seq_len()),
	function (x_) {

		sumlist <- function (x) sum( unlist(x) )

		x_ <- as.list(x_)
		( sumlist %apply% list(list(x_)) == do.call(sum, x_) ) &&
		( sumlist %apply% "x_" == do.call(sum, x_) )
	}
)

context("mcArguments: normal cases")

register$add(
	"mcArguments", type = "test",
	version = "0.4", is_finished = FALSE
)

forall(info = "mcArguments captures arguments of named function",
	list(x_ = r_integers()),
	function (x_) {

		f = function (x) {
			mcArguments()
		}
		identical( f(x_)$x, x_ ) 
	}
)

forall(info = "mcArguments captures arguments of anonymous function",
	list(x_ = r_integers()),
	function (x_) {

		identical(
			(function (x) {
				mcArguments()
			})(),
			x_)

	}
)

forall(info = "mcArguments returns both the defaults and arguments",
	list(x_ = r_integers(), z_ = r_integers()),
	function (x_, z_) {

		identical(
			(function (x, y = 1, z) {
				mcArguments()
			})(x_, z = z_),
			list(x = x_, y = 1, z_ = z_))

	}
)

context("mcClosure: normal cases")

register$add(
	"mcClosure", type = "test",
	version = "0.4", is_finished = FALSE
)

forall(info = "mcClosure converts primitives, with correct arguments",
	list(func_ = r_functions),
	function (func_) {
		
		res <- mcClosure(func_)
		a <- mcParameters(res); b <- mcParameters(res)

		(!is.primitive(res)) &&
		all(names(a) == names(b))
	},
	given = function (func_) {
		is.primitive(func_)
	}
)