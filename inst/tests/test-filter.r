
context("filter: normal cases")

forall(info = "mcFilter (a -> TRUE) -> x is equal to x",
	   list(x_ = r_seq_len(), paropts_ = r_paropts()),
	   function (x_, paropts_) {
	   	identical(
	   		mcFilter(function (...) TRUE, x_, paropts_), x_)
	   }
)
forall(info = "mcFilter (int -> TRUE) -> x is equal to the odds in x",
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
