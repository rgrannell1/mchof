
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

forall(info = "mcPlus, mcMinus, mcMultiply, mcDivide, mcEqual, 
	mcNotEqual returns same value as equivelant operator",
	list(a_ = r_seq_len()),
	function (a_) {
		
		f <- function (n) n^2 - 10
		g <- function (n) n^3 - 20
		
		res_1 <- all_equal( list( mcPlus(f, g)(a_), f(a_) + g(a_) ))
		res_2 <- all_equal( list( mcMinus(f, g)(a_), f(a_) - g(a_) ))
		res_3 <- all_equal( list( mcMultiply(f, g)(a_), f(a_) * g(a_) ))
		res_4 <- all_equal( list( mcDivide(f, g)(a_), f(a_) / g(a_) ))
		res_5 <- all_equal( list( mcEqual(f, g)(a_), f(a_) == g(a_) ))
		res_6 <- all_equal( list( mcNotEqual(f, g)(a_), f(a_) != g(a_) ))
		
		all(res_1, res_2, res_3, res_4, res_5, res_6)
	}
)
