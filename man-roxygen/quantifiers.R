#'
#' @param f a unary function that returns a boolean value, or a string
#' giving the name of such a function.
#' @param x a list or vector.
#' @param paropts a list of parameters to be handed to 
#'    mclapply (see \link{mchof}).
#'
#' @section Special Cases:
#'
#' This function applies \code{f} to each element of \code{x}, and coerces the results to a 
#' \code{TRUE} or \code{FALSE} value. If an \code{NA} value is obtained this is then coerced to a 
#' \code{FALSE} value. This is usually the desired behaviour, but if the user wants
#' \code{NA} values to be converted to \code{TRUE} then they can wrap the input function \code{f} with 
#' \link{mcBoolean}.
#'
#' All quantifiers return \code{NULL} when x = \code{NULL}. When \code{x} is another length-zero value 
#' mcAll returns \code{TRUE}, and mcOne and mcAny return \code{FALSE}. The reason that mcAll returns
#' \code{TRUE} for length-zero data is for consistency with the base function \code{all}.

#'
#' @family quantifiers