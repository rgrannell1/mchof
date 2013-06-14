#' @details This function works hard to try preserve the formals arguments
#' of its input functions in the composite output function. There are 
#' ways in which formals might be outputted
#'
#' 1, If the function f and g have the same parameter names and the same
#' default arguments - in the same order - then the output function will 
#' preserve the parameters and default arguments of f/g
#'
#' 2, If the function f and g have the same parameter names - in the same order -
#' but have different default arguments, then the output function will
#' preserve only the parameters of f/g
#'
#' 3, If the parameters of f and g differ, or their order is shuffled then
#' the output function uses ellipses for as parameters.
#'
#' if any of the input function are primitive (such as '+') then the output function 
#' uses ellipses for parameters.