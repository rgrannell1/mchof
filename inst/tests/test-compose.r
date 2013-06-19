
context("compose: normal cases")

forall(
	info = "check that linear composition is equivelant to multiplication",
	list(
		n_ = r_integers(), m_ = r_integers(),
		x_ = r_integers(), l_ = r_integers()),
	function (n_, m_, l_, x_) {
		
		f <- function (x) n_ * x
		g <- function (x) m_ * x
		h <- function (x) l_ * x
		
		(f %of% g %of% h)(x_) == n_ * m_ l_ * x_
	}	
)
