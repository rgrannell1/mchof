
binary_functional <- function (f, g, operator, func_call) {

	ISSUE("rename binary_functional")

	missing(f) %throws% messages$function_is_required(func_call, "f")
	missing(g) %throws% messages$function_is_required(func_call, "g")		

	operator <- match.fun(operator)

	rm(func_call)
	
	combine_formals(
		function () {			
			operator( f(params_), g(params_) )
		}, f, g)
}
