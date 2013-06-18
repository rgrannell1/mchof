
is_list0 <- function (x) {
	is.list(x) && length(x) == 0
}
is_integer0 <- function (x) {
	is.integer(x) && length(x) == 0
}

any_unnamed <- function (x) {
	any(names(x) == "")
}
any_duplicated <- function (x) {
	any(duplicated(x))
}

type_identity <- function (x) {
	stopifnot(is.vector(x))

	if (is.list(x)) list() else x[0] 
}

curry <- function(FUN,...) {
	# from stack overflow
	.orig = list(...)
	function(...) do.call( FUN,c(.orig,list(...)) )
}

is_boolean <- function (x) {
	# is the value true or false
	
	length(x) > 0 && is.logical(x) && !is.na(x)
}

get_cores <- function (paropts) {
	# check paropts and option(mc.cores) for cores to use
	
	if (!is.null(paropts) && 'mc.cores' %in% names(paropts)) {
		abs(paropts$mc.cores)
	} else if (!is.null(getOption('mc.cores')))  {
		abs(getOption('mc.cores'))
	} else 1
}

accWhile <- function (func) {
	# ( [a] -> NULL | [b] ) -> [b]
	# accumulates a list of the return values
	# of func in a sublist, until func returns NULL.

	acc <- list()
	res <- func (acc)

	while (!is.null(res)) {
		acc <- c(acc, list(res))
		res <- func(acc)
	}
	acc
}
