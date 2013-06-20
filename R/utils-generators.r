
Generator <- function (f) {
	# create a thunk that can be 
	# repeatedly invoked by forall

	list(structure(
		list(f = f),
		class = "generator"
	))
}

pick_len <- function (n=100) {
	sample(seq_len(n), size = 1)
}
pick_one <- function (x) {
	sample(x, size = 1)
}
invoke_gen <- function (gen, n = 1) {
	replicate(n, gen[[1]]$f(), simplify = FALSE)
}

# generate corner cases: values that are likely to 
# require special consideration by the test function
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

Rand <- list()

Rand$empty <- list(
	vector = Generator(
		function () {
			pick_one( list(
				integer(0), character(0), complex(0),
				raw(0), numeric(0), logical(0)) ) 
	}),
	list = Generator( function () list() )
)

Rand$elem <- list(
	int = Generator( function () pick_one(-500:500) ),
	bool = Generator( function () pick_one(c(TRUE, FALSE)) ),
	word = 	Generator( function () {
		paste0(
			letters[ sample(1:26, size = pick_len(20), replace = FALSE) ],
			collapse = "")
	} ),
	letter = Generator( function () letters[ pick_one(1:26) ] )
)

Rand$vect = list(
	vector = function (g) {
		Generator( function () sapply( pick_len(20), invoke_gen(g) ) )
	}
)

Rand$list = list(
	
)
