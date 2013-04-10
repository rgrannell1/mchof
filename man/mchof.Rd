\docType{package}
\name{mchof}
\alias{mchof}
\alias{mchof-package}
\title{mchof: multicore higher-order-functions.}
\description{
mchof (pronounced mac'hoff) provides easy-to-use, consistent parallel implementations of common higher-order functions. Map-like functions - such as those found in plyr - are widely known and loved by useR's. This package aims to bring parallel, predictable and simple implementations of higher order functions found in other languages to R.
}

\section{Functions}{

\describe{
	\item{}{\link{mcFilter}} mcFilter extracts the elements of a vector or list for which the function f returns TRUE.
	\item{}{\link{mcPosition}} Returns the index of the first (or last) position in a vector or list matching a predicate function.
	\item{}{\link{mcFind}} Returns the value of the first element of x that meets the predicate f.
	\item{}{\link{mcReduce}} A parallel implementation of Reduce that works for binary associative functions.
	\item{}{\link{mcZipWith}} mcZipWith takes n lists/vectors, generates a list of n element lists, and returns the result of mapping f over this new list.
	\item{}{\link{mcZip}} mcZip takes n lists/vectors, and generates a list of n element lists. It is a special case of mcZipWith
	\item{}{\link{mcUnzipWith}} mcUnzipWith is the inverse function of mcZipWith; it takes a list of n element lists, makes returns n lists and applies a function to these lists before returning these lists
	\item{}{\link{mcUnzip}} mcUnzip is the inverse function of mcZip; it takes a list of n element vectors, and returns n lists. mcUnzip (mcZip (x)) |-> x
	\item{}{\link{mcPartition}} mcPartition takes n lists/vectors, generates a list of n element lists, and returns the result of mapping f over this new list.
}
}

\section{Parallel}{
to make an mchof function run in parallel, you can simply add \code{list(mc.cores = 2)}, or however many functions you want to add. The full list of parallel options available are given in the documentation for the function \code{mclapply} in the \code{parallel} library.
}
