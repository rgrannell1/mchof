
context("zip & unzip: normal cases")

f <- function (x, paropts = NULL) {
	mcUnzip(do.call(mcZip, c(x, list(paropts = paropts))), paropts)
}
g <- function (x, paropts = NULL) {
	do.call(mcZip, c(mcUnzip(x, paropts), list(paropts = paropts)))
}
mcZipVar <- function (x, paropts) {
	do.call(mcZip, c(x, list(paropts = paropts)))
}

forall(info = "zip is an approximate inverse of unzip",
	list(x_ = c(r_int_tuples(), r_empty_list_tuples()), paropts_ = r_paropts()),
	function (x_, paropts_) {

		all_equal(list(x_, f(x_, paropts_), g(x_, paropts_)))
	}
)

forall(info = "the length of the outputted tuples is uniform", 
	list(x_ = r_flat_no_null(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		all_equal(sapply(mcZipVar(x_, paropts_), length)) &&
		all_equal(sapply(mcUnzip(x_, paropts_), length))	
	}
)

forall(info = "structure of the output is preserved for tuples of list( )",
	list(x_ = r_empty_list_tuples(), paropts_ = r_paropts()),
	function (x_, paropts_) {

		all_equal(list(
			x_,
			f(x_, paropts_), 
			g(x_, paropts_) ))
	}
)

forall(info = "check that the value of the output is correct",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		
		mcZipWith('+', x_, x_) == (x_ + x_) &&
		mcUnzipWith('+', list(x_, x_)) == (x_ + x_)
	}
)




