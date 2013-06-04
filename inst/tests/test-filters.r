
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

denullify <- function (x) {
	# remove the null values from x
	
	null_ind <- which( sapply(x, is.null) )
	if (length(null_ind) > 0) x[-null_ind] else x
}
all_equal <- function (x) {
	length(unique(x)) == 1	
}

forall(
	list(x_ = r_flat_no_null(1000), paropts_ = r_paropts(4)),
	info = "mcFilter ~ !mcReject ~ mcPartition",
	function (x_, paropts_) {
		
		res_0 <- denullify(x_)
		res_1 <- mcFilter(Negate(is.null), x_, paropts_)
		res_2 <- mcReject(is.null, x_, paropts_)
		res_3 <- mcPartition(is.null, x_, paropts_)[[2]]

		all_equal(list(res_0, res_1, res_2, res_3))

	}, opts = list(time = 2)
)


context("reject: normal cases")

forall(info = 'reject FALSE x -> x',
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		identical(
			mcReject(function (...) FALSE, x_, paropts_), x_)
	}
)
forall(info = 'given a function that returns true for odd ints, returns even ints',
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

context("mcPartition: normal cases")

forall(info = "partition (a -> true) -> x is equal to x",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		res <- mcPartition(function (x) TRUE, x_)
		identical( res[[1]], x_ )
	}
)
forall(info = "if x is a list, an empty slot is list()",
	list(x_ = r_flatlist(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		res <- mcPartition(function (x) TRUE, x_)
		identical( res[[2]], list() )
	},
	given = function (x_, paropts_) is.list(x_)
)
forall(info = "if x is an int vector, an empty slot is integer()",
	list(x_ = r_seq_len(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		res <- mcPartition(function (x) TRUE, x_)
		identical(res[[2]], integer(0))
	}
)
forall(info = "if x is a char vector, an empty slot is integer()",
	list(x_ = r_letters(), paropts_ = r_paropts()),
	function (x_, paropts_) {
		res <- mcPartition(function (x) TRUE, x_)
		identical(res[[2]], character(0))
	}
)


