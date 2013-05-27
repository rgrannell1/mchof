
context("test that quantifiers work for normal cases")

prime_property <- function (n) {
	# a property not true of most numbers
	
	is_prime <- function (n) {
		all( as.logical(n %% 2:(n-1)) )
	}
	is_prime(n) && is_prime(n^2 - 1)
}

forall(info = 'any is true when all is true', 
	list(x_ = r_int_vectors(), paropts_ = r_paropts())
	function (x_, paropts_) mcAny(prime_property, x_, paropts_),
	given = function (x_, paropts_) {
		mcAll(prime_property, x_, paropts_)
	}
)

forall(info = 'any is true when one is true & when all is false', 
	list(x_ = r_int_vectors(), paropts_ = r_paropts())
	function (x_, paropts_) {
		mcOne(prime_property, x_, paropts_)
	}
	given = function (x_, paropts_) {
		!mcAll(prime_property, x_, paropts_) &&
		mcAny(prime_property, x_, paropts_)
	}
)
