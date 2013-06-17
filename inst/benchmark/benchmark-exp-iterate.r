		
benchmark$exp$iterate <- ( function () {
	
	iterateWhile_1 <- function (f, p, x) {
		while( !p(x) ) x <- f(x)
		x
	}
	iterateWhile_2 <- function (f, p, x) {
		repeat {
			if (p(x)) break
			x <- f(x)
		}
	}
	iterateWhile_3 <- function (f, p, x) repeat{if(p(x)) break;x<-f(x)}

	f <- function (n) n + 1
	
	mcZipWith(
		mcExplode(function (x) {
			list(test = x[[1]], control = x[[2]], name = x[[3]])	
		}),
		list(
			repeat_loop = function (x) {
				p <- function (n) n == max(x)
				iterateWhile_2(f, p, 1)
			},
			repeat_minified = function (x) {
				p <- function (n) n == max(x)
				iterateWhile_3(f, p, 1)
			}),
		list(
			repeat_loop = function (x) {
				p <- function (n) n == max(x)
				iterateWhile_1(f, p, 1)
			},
			repeat_minified = function (x) {
				p <- function (n) n == max(x)
				iterateWhile_1(f, p, 1)
			}),
		c("repeat_loop", "repeat_minified")
	)
	
} )()

