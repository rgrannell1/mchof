
# sum the vector 1:10 using each function in turn
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcFoldl('+', 0, 1:10)
mcFoldr('+', 0, 1:10)
mcReduce('+', 1:10)
mcReducl('+', 1:10)
mcReducer('+', 1:10)

# many common functions can be defined
# as a fold across a vector
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

sum_1 <- function (x) {
	mcReduce('+', 0, x)
}

as_list_1 <- function (x) {

	mcFoldl(
		function (acc, new) {
			c(acc, as.list(new))
		},
		list(), x)
}
map_1 <- function (f, x) {

	mcFoldl(
		function (a, b) {

			c(a, list( f(b) ))

		}, list(), x
	)
}
 
# even filter can be defined using a foldl
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

filter <- function (f, x) {
	mcFoldl(
		function (a, b) {

			if ( f(b) ) c(a, list(b)) else a 
		}, list(), x
	)
}

# mapReduce! A toy example in which
# raw data is preprocessed and then aggregated
# aggregation can be parallelised here because 
# the the data taken by the first parameter of 
# f and the second parameter are of the same "kind"
#=#=#=#=#=#=#=#=#=#=#=#=#=#=##=#=#=#=#

mcReduce("c",
	Map(
		function (url_data) {

			if (url_data$hits > 1000000) {
				# assume the data was spoofed
				list()
			} else list(url_data)
		},
		list(
			list(url = "cran.r-project.org", hits = 32),
			list(url = "crantastic.org", hits = 100),
			list(url = "en.wikipedia.org/wiki/R_%28programming_language%29", hits = 100),
			list(url = "fakespammyspam.org", hits = 9000000),
			list(url = "spamspamspameggs&spam.spam", hits = 9000000)
		)
	)
)
