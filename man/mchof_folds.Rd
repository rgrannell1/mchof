\name{mcFold}
\alias{mcFold}
\alias{mcReduce}
\title{Fold-like Higher-Order-Functions}
\usage{
  mcFold(f, x, xs, paropts = NULL)

  mcReduce(f, xs, paropts = NULL)
}
\arguments{
  \item{f}{a binary function that takes two of "a thing"
  and returns one of a "thing" (see details).}

  \item{x}{a list or vector.}

  \item{z}{an initial value to be prepended to \code{x}}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see \link{mchof}).}
}
\description{
  \code{mcFold} applies an associative binary function
  \code{f} to a list or vector \code{x}, returning a single
  value. If x is length zero then an initial value is
  returned.

  \code{mcReduce} applies an associative binary function
  \code{f} to a list or vector \code{x}, returning a single
  value.
}
\details{
  \code{mcFold} and \code{mcReduce} function can be used as
  a parallel alternative to the non-parallel folding
  functions if and only if the function \code{f} is
  associative; that is

  \deqn{(a f b) f c = a f (b f c)}

  where \eqn{a, b, c} are values that \code{f} takes. For
  example, plus is an associative binary operator, since

  \deqn{(a + b) + c = a + (b + c)}

  for any number \eqn{a, b, c}. Minus does not have this
  property, so it is not suitable for use with
  \code{mcFold}. Only associative binary functions can be
  folded or reduced in parallel.

  Formally the combination of an associative binary
  operator, an identity element for that operator and a set
  (x) is known as a monoid; the function \code{f} has a
  type signature of \eqn{[A] -> [A] -> [A]}. A likely
  source of errors when using \code{mcFold} or
  \code{mcReduce} is using a function without this type
  signature (ie. a function that takes two of a thing, and
  returns one of a thing).

  with \code{mcFold} it useful to use the identity of
  \code{f} as first, as it can make it possible to simplify
  \code{f}. For example, lists have an identity element of
  \code{list()} when concatenated, and integers have an
  identity of 0 under addition.
}
\section{Special Cases}{
  when x is NULL, NULL is automatically returned (since
  NULL falls throught all mchof functions without being
  interperated as meaningful data). If x is a length-zero
  value such as list() or integer(0) then \code{mcFold}
  returns \code{first}, and \code{mcReduce} automatically
  returns \code{x}.

  \code{mcReduce} returnes length-one values because a
  binary function cannot be applied to a single value, so
  the value is presumed to be already fully "reduced".
}
\examples{

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
	mcFold('+', 0, x)
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
			list(url = "en.wikipedia.org/wiki/R_\%28programming_language\%29", hits = 100),
			list(url = "fakespammyspam.org", hits = 9000000),
			list(url = "spamspamspameggs&spam.spam", hits = 9000000)
		)
	)
)
}

