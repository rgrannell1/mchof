
is_ten <- function (n) n == 10
not_ten <- mcNot(is_ten)
not_ten(10)

# cancel out mcNot with a second mcNot

mcNot(mcNot( function () TRUE ))()

# select the non null elements from a list

mcSelect(
	mcNot('is.null'),
	list(1, NULL, 2,3,4)
)


