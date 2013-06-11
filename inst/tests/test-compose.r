
context("compose: normal cases")

forall(
	info = "check that linear composition is equivelant to multiplication",
	list(n_ = r_integers(), m_ = r_integers(), x_ = r_integers()),
	function (n_, m_, x_) {
		
		f <- function (x) n_ * x
		g <- function (x) m_ * x
		
		(f %of% g)(x_) == n_ * m_ * x_
	}	
)

