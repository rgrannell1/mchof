\name{mcFind}
\alias{mcFind}
\title{mcFind}
\usage{
  mcFind(f, x, right = FALSE, nomatch = NULL)
}
\arguments{
  \item{f}{a unary function that returns either \code{TRUE}
  or \code{FALSE}}

  \item{x}{a vector}

  \item{right}{a boolean value. Should the first
  \code{TRUE} or last \code{FALSE} element matching
  \code{f} be returned? Defaults to \code{FALSE}}

  \item{nomatch}{the variable that is returned if no
  elements such that \code{f(element) = TRUE} are found in
  \code{x}. Defaults to \code{NA}}

  \item{paropts}{a list of parameters to be handed to
  mclapply (see details and \code{\link{mclapply}})}
}
\description{
  Returns the value of the first element of x that meets
  the predicate f.
}

