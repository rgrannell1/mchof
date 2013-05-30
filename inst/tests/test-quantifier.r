
context("quantifiers: normal cases")

prime_property <- function (n) {
	# a property not true of most numbers
	
	is_prime <- function (n) {
		all( as.logical(n %% 2:(n-1)) )
	}
	is_prime(n) && is_prime(n^2 - 1)
}

closure_over_multiplication <- function (set) {
	is.numeric(prod(set))
}

forall(info = 'all is true implies any is true', 
	list(x_ = r_int_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) mcAny(prime_property, x_, paropts_),
	given = function (x_, paropts_) {
		mcAll(prime_property, x_, paropts_)
	}
)

forall(info = 'one is true implies all is false and any is true', 
	list(x_ = r_int_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcOne(prime_property, x_, paropts_)
	},
	given = function (x_, paropts_) {
		!mcAll(prime_property, x_, paropts_) &&
		mcAny(prime_property, x_, paropts_)
	}
)

forall(info = "all is true implies any is true", 
	list(x_ = r_int_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) mcAny(prime_property, x_, paropts_),
	given = function (x_, paropts_) {
			mcAll(prime_property, x_, paropts_)
	}
)

forall(info = "a x b always yields an integer",
	list(x_ = r_int_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcAll(closure_over_multiplication, x_, paropts_)
	}	
)
