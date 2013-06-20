
# how much overhead does input validation add to 
# each function? What tests are most expensive?
		
benchmark_exp$overhead <- ( function () {
	
	fold_control <- function (x) Reduce(function (a, b) 1, x)

	mcZipWith(
		mcExplode(function (x) {
			list(test = x[[1]], control = x[[2]], name = x[[3]])	
		}),
		list(
			stripped = function (x) {

				f <- function (...) 1
				ind <- len_x <- length(x)

				while (ind > 0) {
				    z <- f( x[[ind]], z )
				    ind <- ind - 1
				}
				z
			},
			heavy = function (x) {
				
				f <- function (...) 1
				func_call <- "mcReducel(f, x)"

				missing(f) %throws% messages$function_is_required(func_call, "f")
				missing(x) %throws% messages$vector_is_required(func_call, "x")
					
				f <- match.fun(f)
				if (length(x) < 2) return (x)
				is.factor(x) %throws% messages$was_factor(func_call, x, "x")
				
				f <- function (...) 1
				ind <- len_x <- length(x)

				while (ind > 0) {
				    z <- f( x[[ind]], z )
				    ind <- ind - 1
				}
				z
			}
		),
		list(
			stripped = fold_control,
			heavy = fold_control
		),
		c("stripped", "heavy")
	)
	
} )()

