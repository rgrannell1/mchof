
context("mcPartial: normal cases")


forall(info = "partial application a binary to unary function works",
	list(
		a_ = r_integers(),
		b_ =  r_integers(),
		operator_ = list("+", "*", "/", "-")),	
	function (a_, b_, operator_) {
	
		operator <- function (x, y) {
			match.fun(operator_)(x, y)
		}

		g <- mcPartial(operator, list(x = a_))

		print(g(b_))

		g(b_) == operator(a_, b_)
	}
)

forall(info = "iterative partial application works", 
	list(
		a_ = r_integers(), b_ =  r_integers(),
		c_ = r_integers(), d_ =  r_integers()),
	function (a_, b_, c_, d_) {
		
		f <- function (x, y, z, w) x + y + z + w

		g <- mcPartial(f, list(x = a_))
		h <- mcPartial(g, list(y = b_)) 
		e <- mcPartial(h, list(z = c_)) 

		(e(d_) == a_ + b_ + c_ + d_)
	}
)

forall(info = "iterative partial application shortens the formals one at a time", 
	list(
		a_ = r_integers(), b_ =  r_integers(),
		c_ = r_integers(), d_ =  r_integers()),
	function (a_, b_, c_, d_) {
		
		f <- function (x, y, z, w) x + y + z + w
		g <- mcPartial(f, list(x = a_))
		h <- mcPartial(g, list(y = b_))
		e <- mcPartial(h, list(z = c_))
		
		length( names(formals(f)) ) == 4 &&
		length( names(formals(g)) ) == 3 &&
		length( names(formals(h)) ) == 2 &&
		length( names(formals(e)) ) == 1
	}
)
