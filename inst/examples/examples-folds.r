
# sum the vector 1:10 using each function in turn
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcFoldl('+', 0, 1:10)
mcFoldr('+', 0, 1:10)
mcReduce('+', 1:10)
mcReducl('+', 1:10)
mcReducer('+', 1:10)

# define as_list using a fold
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

as_list <- function (x) {

	mcFoldl(
		function (acc, new) {
			c(acc, as.list(new))
		},
		list(), x)
}