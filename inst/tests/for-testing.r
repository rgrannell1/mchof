
axiom <- function (info = '', always, unless = TRUE, where) {

	Function <- function (parametres = alist(), body = {}) {
		# a dynamic form of function that assumes it's inputs 
		# are arguments to be substituted as variables

		tmp <- function () {}
		formals(tmp) <- parametres
		body(tmp) <- match.call()$body		
		tmp
	}

	parametres <- structure (
		# create an alist for unless and always
		
		Map( function (x) NULL, seq_along(where)),	
		names = Map(
			function (name) as.symbol(name),	
			as.list(names(where))
	))

	expr_func <- Function (parametres, {
		
		print('')
		expr
	})
	unless_func <- Function (parametres, {
		unless
		
		print('l')
		
	})
	
	composed <- Function (parametres, {
	
		args <- as.list(match.call())[-1]
		
		if (!do.call(unless_func, args)) {
			do.call(expr_func, args)
		}
		
	})
	
	test_all_cases <- function (rule, cases) {
		# check that the rules hold for all 	
		
		combos <- do.call (
			expand.grid,
			Map (seq_len, Map (length, cases)))
		
		apply(combos, 1, function (x) {	
			do.call(rule, as.list(x))
		})	
	}
	test_all_cases(composed, where)
}

axiom ('',
	always = a + b + c > max(a, b, c),	
	unless = min(a, b, c) == 0,
	where = list (
		a = seq_len(5),		
		b = seq_len(5),
		c = seq_len(5)
	)
)
