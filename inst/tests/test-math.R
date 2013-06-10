
context("math functions: normal cases")

forall(info = "mcLarger and mcSmaller returns same value as operator",
	list(a_ = r_seq_len()),
	function (a_) {
		
		f <- function (n) n^2 - 10
		g <- function (n) n^3 - 20
		mcLarger(f, g)(a_) == all(f(a_) > g(a_)) &&
		mcSmaller(f, g)(a_) == all(f(a_) < g(a_))	
	}
)

forall(info = "mcPlus, mcMinus, mcMultiply, mcDivide returns same value as operator",
	list(a_ = r_seq_len()),
	function (a_) {
		
		f <- function (n) n^2 - 10
		g <- function (n) n^3 - 20
		
		all_equal( list( mcPlus(f, g)(a_), f(a_) + g(a_) )) &&
		all_equal( list( mcMinus(f, g)(a_), f(a_) - g(a_) ))
		all_equal( list( mcMultiply(f, g)(a_), f(a_) * g(a_) ))
		all_equal( list( mcDivide(f, g)(a_), f(a_) / g(a_) ))
	}
)
