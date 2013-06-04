
context("quantifiers: normal cases")

prime_property <- function (n) {
	# a property not true of most numbers
	
	is_prime <- function (n) {
		all( as.logical(n %% 2:(n-1)) )
	}
	is_prime(n) || is_prime(n^2 - 1)
}

closure_over_multiplication <- function (set) {
	is.numeric(prod(set))
}
is_associative <- function (set) {
	Reduce(get('+'), set, right = TRUE) ==
	Reduce(get('+'), set, right = FALSE)
}

forall(info = 'all is true implies any is true', 
	list(x_ = r_int_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) mcAny(is.numeric, x_, paropts_),
	given = function (x_, paropts_) {
		mcAll(is.numeric, x_, paropts_)
	}
)

forall(info = 'one is true implies any is true', 
	list(x_ = r_int_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcAny(prime_property, x_, paropts_)
	},
	given = function (x_, paropts_) {		
		mcOne(prime_property, x_, paropts_)
	}
)

forall(info = "a x b always yields an integer",
	list(x_ = r_int_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcAll(closure_over_multiplication, x_, paropts_) &&
		mcAny(closure_over_multiplication, x_, paropts_) 
	}	
)

forall(info = "(a + b) + c == a + (b + c)",
	list(x_ = r_int_vectors(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		mcAll(is_associative, x_, paropts_) &&
		mcAny(is_associative, x_, paropts_) 
	}
)

forall(info = "one element in 1...n is equal to n",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		n <- sample(x_, size = 1)
		
		mcAny(function (x) x == n, x_, paropts_) &&
		mcOne(function (x) x == n, x_, paropts_) 
	}
)
