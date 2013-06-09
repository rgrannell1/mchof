
repl <- function (f, n) {
	Map(f, seq_len(n))
}
pick_len <- function (n=100) {
	sample(seq_len(n), size = 1)
}

r_integers <- function (n=100) {
	sample(-100:100, size = n, replace = TRUE)
}
r_letters <- function (n=100) {
	letters[(r_integers(n) %% 25) + 1]
}
r_words <- function (n=100) {
	repl(
		function (...) {
			size <- sample(1:20, size = 1)
			paste0(r_letters(size), collapse = '')
		}, n)
}
r_null <- function (n=100) {
	NULL
}
r_null_list <- function (n=100) {
	Map(function(...) NULL, seq_len(n))
}
r_vector_0 <- function (n=100) {
	sample(
		list(
			integer(0), character(0), complex(0),
			raw(0), numeric(0), logical(0)),
		size = n, replace = TRUE)
}
r_flatlist <- function (n=100) {
	Map(
		function (...) {

			func <- list(r_integers, r_letters, r_words, r_null)[[ pick_len(4) ]]
			func(pick_len(20))

		}, seq_len(n))
}
r_flat_no_null <- function (n=100) {
	Map(
		function (...) {

			func <- list(r_integers, r_letters, r_words)[[ pick_len(3) ]]
			func(pick_len(20))

		}, seq_len(n))	
}
r_paropts <- function (n=100) {
	list(
		list(),
		list(mc.cores = 1), list(mc.cores = 2), list(mc.cores = 3),
		list(mc.cores = 4), list(mc.cores = 5), list(mc.cores = 6),
		list(mc.cores = 7), list(mc.cores = 8), list(mc.cores = 9))
}
r_tuple_list <- function (n=100) {
	tuple_length <- pick_len(20)
	
	Map(
		function (...) {
			r_flatlist(tuple_length)
		},
		seq_len(n))
}
r_int_tuples <- function (n=100) {
	
	tuple_length <- pick_len(20)
	
	Map(
		function (...) {
			list(repl(function (...) r_integers(tuple_length), tuple_length))
		},
		seq_len(n))
}
r_seq_len <- function (n=100) {
	repl(
		function (...) 1:pick_len(20), n)
}
r_int_vectors <- function (n=100) {
	repl(
		function (...) r_integers(pick_len(20)), n)
}
r_small_named_list <- function (n=100) {
	repl(
		function (...) {
			structure(list(r_integers(10)), names = r_letters(1))
		}, n)
}
r_empty_list_tuples <- function (n=100) {
	tuple_length <- pick_len(20)
	
	Map(
		function (...) {
			Map(
				function (...)	{
					repl(function (...) list(), tuple_length)
				},
				seq_len(pick_len(n)))	
		},
		seq_len(pick_len(n)))
}
r_typed_vectors <- function (n=100) {
	repl(
		function (...) {
			sample(c(
				list(r_integers(pick_len(20))),
				list(r_letters(pick_len(20)))), size = 1)[[1]]
		}, n)	
}
