
Axiom <- function (info = '', always, unless = TRUE, where) {
	# asdfbi
	
	test_all_cases <- function (rule, cases) {
		# check that the rules hold for all 	
		
		combos <- do.call (
			expand.grid,
			Map (seq_len, Map (length, cases)))
		
		Map (rule, combos)	
	}
	
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

	test_all_cases(
		Function (parametres, {
			
			if (!Function (parametres, { unless }) &&
				Function (parametres, { always })) {
				
				stop (info, 'ghastly error')
				
			}
		}), where
	)
}

Axiom ('',
	always = a + b + c > max(a, b, c),	
	unless = min(a, b, c) == 0,
	where = list (
		a = seq_len(5),		
		b = seq_len(5),
		c = seq_len(5)
	)
)
