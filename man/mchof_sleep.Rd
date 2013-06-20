\name{mcSleep}
\alias{mcSleep}
\alias{mcTimer}
\title{Functions for Controlling Temporal Flow of a Program}
\usage{
  mcSleep(f, n)

  mcTimer(n)
}
\arguments{
  \item{\code{f}}{a function or a string giving the name of
  a function.}

  \item{\code{n}}{a number that is interperated as seconds}
}
\description{
  1
}
\details{
  1
}
\examples{

slow_mean <- mcSleep(f = mean, n = 1)
slow_mean(1)

slow_hello <- mcSleep(
	function () print("good morning! *yawn*"),
	n = 1)
slow_hello()

}

