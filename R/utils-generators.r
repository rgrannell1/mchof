
pick_len <- function (n=100) {
	sample(seq_len(n), size = 1)
}
gen_invoke <- function (gen, n = 1) {
	# invoke the generator n times

	replicate(n, gen[[1]]$f(), simplify = FALSE)

}

# generate corner cases: values that are likely to 
# require special consideration by the test function
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

r_vector_zero <- Generator (
	function () {
		sample(
			list(
				integer(0), character(0), complex(0),
				raw(0), numeric(0), logical(0)),
			size = 1)
	}
) 

r_collection_zero  <- Generator (
	function () {
		sample(
			list(
				integer(0), character(0), complex(0),
				raw(0), numeric(0), logical(0), list()),
			size = 1)
	}
) 

r_length_zero <- Generator (
	function () {
		sample(
			list(
				integer(0), character(0), complex(0),
				raw(0), numeric(0), logical(0), list(), NULL),
			size = 1)
	}
) 

# generate values: numbers, words, letters, which are 
# elements of larger structures such as lists or vectors
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

r_integers <- Generator(
	function () sample(-500:500, size = 1)
)
r_letters <- Generator(
	function () letters[ sample(1:26, size = 1) ]
)
r_words <- Generator(
	function () {
		chars <- pick_len(20)

		paste0(
			letters[ sample(1:26, size = chars, replace = FALSE) ],
			collapse = "")

	}
)

## collection generator functionals, which take element generators and 
## return generators for vectors and lists
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

gen_to_vector <- function (element_gen) {
	# takes a generator for a single 
	# type of vector element, and return a generator
	# for length > 1 vectors of that element

	Generator(
		function () {

			length <- pick_len(20) 
			gen_invoke(element_gen, length)
		}
	)
}

gen_to_flatlist <- function (element_gen) {
	# takes a generator for a single 
	# type of vector element, and return a generator
	# for length > 1 lists of that element

	Generator(
		function () {

			length <- pick_len(20) 
			as.list( gen_invoke(element_gen, length) )
		}
	)
}

## name generator functionals, which take collection
## generator and returns a generator of named collections
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

gen_add_names <- function (collection_gen) {
	# takes a collection generator, and 
	# returns a collection generator that names its 
	# outputs. May contain duplicate values

	Generator(
		function () {

			x <- gen_invoke(collection_gen)
			names(x) <- gen_invoke(r_vect_words, length(x))
			x
		}
	)
}

gen_add_unique_names <- function (collection_gen) {
	# takes a collection generator, and 
	# returns a collection generator that names its 
	# outputs. Each name is unique.
	
	Generator(
		function () {
			
			x <- gen_invoke(collection_gen)
			names(x) <- paste0(
				gen_invoke(r_words), seq_along(x))
			x
		}
	)
}

