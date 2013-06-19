
# find all primes in 1...15

is_prime <- function (n) {
    if (n == 1 || n == 2) FALSE else {
        !any( n %% (2:(n - 1)) == 0)
    }
}

mcSelect(is_prime, 1:15) #or
mcReject(mcNot(is_prime), 1:15)

# find the largest prime factor of 2023

mcReduce(max,
	mcSelect(
		mcAnd(is_prime, function (n) (2023 %% n) == 0) ,
		1:2023) 
)

# find solutions to 2*x + 4*y < 100

mcSelect(
    mcImplode(
		function (x, y) {
			2*x + 4*y < 100
		}
	),
	mcZip(
		sample(1:100, size = 100),
		sample(1:100, size = 100)
	)
)

# Project Euler 8: find the greatest product of 5-consecutive digits
# in a 1000-digit number

# use random data instead of the given number

number <- sample(0:9, size = 1000, replace = TRUE)

indices <- mcReduce(
	function (a, b) {
		if (prod(number[a]) > prod(number[b])) a else b
	},
	mcZipWith(
		"+",
		rep(list(1:5), 995), 1:1000)
)

# the mcZipWith step generates the indices 
# [1,..., 5], [2,..., 6], ... [995, ..., 1000]

number[indices]

# partition a vector into good cops and other kinds
# of cop

mcPartition(
	function (cop) {
		cop == "good cop"
	},
	c("good cop", "bad cop", "good cop", "bad cop", "it's a fair cop!")
)

