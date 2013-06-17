\name{mcAnd}
\alias{mcAnd}
\alias{mcNot}
\alias{mcOr}
\alias{mcXor}
\title{Higher-Order-Functions for Functional Logic}
\usage{
  mcAnd(f, g)

  mcNot(f)

  mcOr(f, g)

  mcXor(f, g)
}
\arguments{
  \item{f}{a function that returns a logical value, or a
  string giving the name of such a function.}

  \item{g}{a function that returns a logical value, or a
  string giving the name of such a function.}
}
\description{
  \code{mcAnd} takes two functions \code{f} and \code{g},
  and returns a composite function. This composite function
  returns \code{f(...) && g(...)}.

  \code{mcNot} takes a function \code{f}, and returns a
  function. This function returns the logical negation of
  \code{f}: if \code{f} returns \code{TRUE} for a value
  \code{x} then \code{mcNot(f)} will return \code{FALSE}
  (and visa versa when \code{f(x) == FALSE}).

  \code{mcOr} takes two functions \code{f} and \code{g},
  and returns a composite function. This composite function
  returns \code{f(...) || g(...)}.

  \code{mcXor} takes two functions \code{f} and \code{g},
  and returns a composite function. This composite function
  returns \code{xor( f(...), g(...) )}. The \code{xor}
  function returns \code{TRUE} if either its first or
  second argument is \code{true}, but not both and not
  neither.
}
\details{
  All of these functions return logical values
  (\code{TRUE}, \code{FALSE}, or \code{NA}) and not boolean
  values (\code{TRUE}, \code{FALSE}). \code{NA} values will
  be obtained if either \code{f} or \code{g} returns
  \code{NA} when called with a particular value. This is
  easily altered by composing the output of \code{mcAnd},
  \code{mcNot}, ... with\code{\link{mcBoolean}}. For
  example,

  \code{finite_number = mcBoolean \%of\% mcAnd(is.numeric,
  is.finite)}

  \code{finite_number} first checks whether its input is
  numeric and finite, and then coerces this result to
  \code{TRUE} or \code{FALSE}.

  The composite functions formals depend on the input
  function(s):

  \enumerate{ \item If the function \code{f} and \code{g}
  have the same parameter names - in the same order - but
  have different default arguments, then the output
  function will preserve only the parameters of
  \code{f}/\code{g}.  \item If the parameters of \code{f}
  and \code{g} differ, or their order is shuffled then the
  output function uses ellipses as parameters.  \item
  primitive functions (such as \code{max}) are supported by
  mchof, and their arguments are processed in a similar way
  to those of closures (standard R functions). } For mcNot
  the parameters of \code{f} are automatically copied to
  the output composite function.
}
\examples{

h <- function (name) name == "cat"
w <- function (name) name == "dog"

cat_or_dog <- mcAnd(h, w)
cat_or_dog("rat")

# example without saving result of mcAnd to 
# intermediate function

f <- function (n) n > 5
g <- function (n) n < 10

mcAnd(f, g)(7)

## pairing mcAnd with mcSelect

mcSelect(
	mcAnd(
		function (x) x < 10,
		function (x) x == 7),
	seq_len(20)
)


}
\keyword{mcAnd}
\keyword{mcNot}
\keyword{mcOr}
\keyword{mcXor}

