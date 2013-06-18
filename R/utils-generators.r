
pick_len <- function (n=100) {
	sample(seq_len(n), size = 1)
}

# generate corner cases: values that are likely to 
# require special consideration by the test function

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

# generate vectors: named, unnamed, and of various types

# unnamed vectors

r_vect_integers <- Generator(
	function () {
		size <- pick_len(20)
		sample(-500:500, size = size, replace = TRUE)
	}
)
r_vect_letters <- Generator(
	function () {
		size <- pick_len(20)
		sample(letters, size = size, replace = TRUE)
	}
)

r_vect_sequence <- Generator(
	function () {
		size <- pick_len(20)
		seq_len(size)
	}
)

# named vectors: some partially, 
# some fully, some without duplicates

r_vect_integers <- Generator(
	function () {
		size <- pick_len(20)
		sample(-500:500, size = size, replace = TRUE)
	}
)
r_vect_letters <- Generator(
	function () {
		size <- pick_len(20)
		sample(letters, size = size, replace = TRUE)
	}
)

r_vect_sequence <- Generator(
	function () {
		size <- pick_len(20)
		seq_len(size)
	}
)
# generate non-nested lists
###########################

r_list_integers <- Generator(
	function () {
		size <- pick_len(20)
		as.list( sample(-500:500, size = size, replace = TRUE) )
	}
)
r_list_letters <- Generator(
	function () {
		size <- pick_len(20)
		as.list( sample(letters, size = size, replace = TRUE) )
	}
)












