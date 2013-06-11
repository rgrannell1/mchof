\name{mcSleep}
\alias{mcSleep}
\title{mcSleep}
\usage{
  mcSleep(f, n)
}
\arguments{
  \item{f}{a function or a string giving the name of a
  function.}

  \item{n}{the amount of time to sleep before executing f,
  in seconds}
}
\value{
  returns a function that calls f after n seconds
}
\description{
  mcSleep takes a function f, and returns a function that
  waits n seconds before calling f
}
\details{
  mcSleep is useful for stalling quick functions that
  execute queries to a website with a rate limit, to avoid
  hitting the rate limit before it refreshes.
}
\examples{

slow_mean <- mcSleep(f = mean, n = 1)
slow_mean(1)

slow_hello <- mcSleep(
	function () print("good morning! *yawn*"),
	n = 1)
slow_hello()

}
\keyword{mcSleep}

