
context("reject: normal cases")

forall(info = 'reject FALSE x -> x',
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		identical(
			mcReject(function (...) FALSE, x_, paropts_), x_)
	}
)
forall(info = 'given a function that returns true for odd ints, returns evn ints',
	   list(x_ = r_seq_len(), paropts_ = r_paropts()),
	   function (x_, paropts_) {
	   	
	   	is_odd <- function (x) {
	   		as.logical(x %% 2)
	   	}
	   	identical(
	   		mcReject(is_odd, x_, paropts_),
	   		mcFilter(Negate(is_odd), x_, paropts_))
	   }
)
