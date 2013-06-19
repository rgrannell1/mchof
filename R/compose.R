
#' Higher-Order-Functions for Composing Functions
#'
#' @description 1
#'
#' @rdname mchof_compose
#' @family mchof-compose
#' @example inst/examples/examples-compose.r
#' @export

mcCompose <- function (f, g) {
	# (a -> b) -> (b -> c) -> (a -> c)
	# return a composite function f o g
	
	func_call <- "mcCompose(f, g)"

	missing(f) %throws% 
		messages$function_is_required(func_call, "f")
	missing(g) %throws% 
		messages$function_is_required(func_call, "g")
	
	f <- match.fun(f)
	g <- match.fun(g)
	
	rm(func_call)
	
	combine_formals(
		function () {
			f(g(params_))
		},
		f, g
	)
}

#' @export

'%of%' <- mcCompose


mcWrap <- function (f, g) {

	func_call <- "mcWrap(f, g)"

}