
pick_one <- function (x) {
	sample(x, size = 1)
}
pick_size <- function (n) {
	sample(seq_len(n), size = 1)
}
rep_sample <- function (x, n) {
	sample(x, size = n, replace = TRUE)
}

# so. many. magic. numbers !!

rInputs <- list(
	integers = function (n) {
		rep_sample(-1000:1000, n)
	},
	floats = function (n) {
		round( runif(n, -1000, +1000), 2)	
	},
	letters = function (n) {
		rep_sample(letters[1:26], n)	
	},
	strings = function (n) {
		
		replicate(n, ( function () {		
			list( paste0(rInputs$letters( pick_size(20) ), collapse = '') )
		} )())
	},
	booleans = function (n) {
		rep_sample(c(TRUE, FALSE), n)		
	},
	logicals = function (n) {
		rep_sample(c(TRUE, FALSE, NA),  n)
	},
	null = function (n) {
		rep_sample(list(NULL), n)	
	},
	
	character_vectors = function (n) {
		replicate(n, list( rInputs$strings( pick_size(20) ) ))
	},
	integer_vectors = function (n) {
		replicate(n, list( rInputs$integers( pick_size(20) ) ))
	},
	float_vectors = function (n) {
		replicate(n, list( rInputs$floats( pick_size(20) ) ))
	},
	
	flatlist = function (n) {
		replicate(n, {
	
			funcs <- list(
				rInputs$integers,
				rInputs$letters,
				rInputs$strings,
				rInputs$booleans,
				rInputs$logicals,
				rInputs$character_vectors,
				rInputs$integer_vectors,
				rInputs$logicals
			)
			test_fun <- pick_one(funcs)[[1]]
			
			list( test_fun(pick_size(20)) )
		})
	},
	flatlist_with_null = function (n) {
		replicate(n, {
				
		})
	}
)
