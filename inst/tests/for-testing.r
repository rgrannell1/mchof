
Axiom <- function (info = '', expr, unless = TRUE, where) {

	test_case <- function (fits_rule, is_exception, case) {
		
		if (!is_exception && !fits_rule(case)) stop(case)	
	}
	
	Function <- function (args = alist(), body = {}) {
		# a dynamic form of function that assumes it's inputs 
		# are arguments to be substituted as variables

		tmp <- function () {}
		formals(tmp) <-  match.call()$args
		body(tmp) <- match.call()$body		
		tmp
	}
	
	args <- structure (
		# create an alist for unless and expr
		Map( function (x) NULL, seq_along(where)),	
		names = Map(
			function (name) as.symbol(name),	
			as.list(names(where))
	))

	is_exception <- Function (args, { unless })
	check_case <- Function (args, { expr })

	# NOW MAP OVER ALL CASES
	
}

Axiom (
	a + b + c > max(a, b, c),	
	min(a, b, c) == 0,
	where = list (
		a = seq_len(5),		
		b = seq_len(5),
		c = seq_len(5)
	)
)
