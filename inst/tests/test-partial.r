
context("mcPartial: normal cases")

forall(info = "partial application a binary to unary function works",
	list(
		a_ = r_integers(), b_ =  r_integers(),
		operator_ = list("+", "*", "/", "-")),	
	function (a_, b_, operator_) {
	
		f <- function (x, y) get(operator_)(x, y)
		g <- mcPartial(f, x = a_)
		g(b_) == get(operator_)(a_, b_)
		
	}
)

forall(info = "iterative partial application works", 
	list(
		a_ = r_integers(), b_ =  r_integers(),
		c_ = r_integers(), d_ =  r_integers()),
	function (a_, b_, c_, d_) {
		
		f <- function (x, y, z, w) x + y + z + w
		g <- mcPartial(f, x = a_)
		h <- mcPartial(g, y = b_) 
		e <- mcPartial(h, z = c_) 
		
		(e(d_) == a_ + b_ + c_ + d_)
	}
)

forall(info = "iterative partial application shortens the formals one at a time", 
	list(
		a_ = r_integers(), b_ =  r_integers(),
		c_ = r_integers(), d_ =  r_integers()),
	function (a_, b_, c_, d_) {
		
		f <- function (x, y, z, w) x + y + z + w
		g <- mcpartial(f, x = a_)
		h <- mcpartial(g, y = b_) 
		e <- mcpartial(h, z = c_) 
		
		length( names(formals(f)) ) == 4 &&
		length( names(formals(g)) ) == 3 &&
		length( names(formals(h)) ) == 2 &&
		length( names(formals(e)) ) == 1
	}
)
