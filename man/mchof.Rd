\docType{package}
\name{mchof}
\alias{mchof}
\alias{mchof-package}
\title{mchof: multicore higher-order-functions}
\description{
  mchof: multicore higher-order-functions
}
\section{Functions}{
  Below is a full list of functions currently included in
  mchof, along with a description of what they do

  \itemize{ \item \code{\link{mcFilter}}: get the elements
  of a list matching a predicate. \item
  \code{\link{mcReduce}}: applies an associative binary
  operator to a list (eg. +, *, rbind), returning a single
  element. \item \code{\link{mcFold}}: applies an
  associative binary operator to a list (eg. +, *, rbind)
  with an initial value, returning a single element. \item
  \code{\link{mcFind}}: get the first element in a list
  matching a predicate. \item \code{\link{mcPosition}}:
  find the first element in a list matching a predicate.
  \item \code{\link{mcZipWith}}: combine n lists into a
  list of n-tuples, apply a function to each element of the
  result. \item \code{\link{mcZip}}: combine n lists into a
  list of n-tuples. \item \code{\link{mcUnzipWith}}: split
  a list of n-tuples into n lists, apply a function to each
  element of the results. \item \code{\link{mcUnzip}}:
  split a list of n-tuples into n lists. \item
  \code{\link{mcAll}}: apply a function f to x, return TRUE
  if f is true for all element in x \item
  \code{\link{mcAny}}: apply a function f to x, return TRUE
  if f is true for any element in x \item
  \code{\link{mcOne}}: apply a function f to x, return TRUE
  if f is true for one element in x \item
  \code{\link{mcReject}}: get the elements of a list not
  matching a predicate. }
}

\section{Parallel Options (Paropts)}{
  As of version 0.3 mchof is build on top of the parallel
  library, which employs forking to create parallel
  processes. Forking is not possible on windows.

  The most important option is mc.cores, which sets the
  number of processes to spawn. See mclapply in the
  parallel library for a more complete description of the
  options that can be used with mchof.

  Note; for obvious reasons FUN or X cannot be set in
  paropts in case they conflict with f and x (arguments
  available to most mchof functions).
}

\section{Multiple Argument Functions}{
  sometimes it is necessary to set optional parameters for
  a function. However, most mchof functions take a fixed
  number of arguments. There are several ways to set
  optional arguments despite this fact.

  the easiest way to add the second parameter is to wrap
  the function in an anonymous function & add the second
  parameter inside the anonymous function.

  it is also useful to create a curried function (a
  function with some parameters substituted) in cases where
  the code is likely to be re-used.
}

\section{NA handling}{
  many functions in mchof take a TRUE/FALSE value. If a
  user's function returns NA this will be converted
  internally to either TRUE or false. Users can override
  this behaviour by converting the return values:

  # TRUE -> TRUE, FALSE -> FALSE, NA -> FALSE

  \code{ as_boolean_one <- function (x) isTRUE(x) }

  # TRUE -> TRUE, FALSE -> FALSE, NA -> TRUE

  \code{ as_boolean_two <- function (x) isTRUE(x) ||
  is.na(x) }

  or alternatively the higher-order functions

  \code{f <- function (func) function (...)
  as_boolean_one(...)}

  \code{g <- function (func) function (...)
  as_boolean_two(...)}
}

