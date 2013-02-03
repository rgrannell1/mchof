#' Filters a list or vector in parallel 
#' 
#' @description mcFilter extracts the elements of a vector for 
#' which the function \code{f} returns true, in parallel
#' 
#' @param f, a unary function that returns either \code{TRUE} or \code{FALSE}
#' @param x, a vector
#' @param paropts, a list of parameters to be handed to 
#'    mclapply (see \code{\link{mclapply}})
#'    
#' 
#'    