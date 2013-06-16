
sentence <- mcPartial(paste, collapse = " ")
sentence(c("this", "is", "a", "curried", "function"))

# increment a list 1..10 with curried plus function

plus <- function (a, b) {
	# + doesn't actually have formal arguments, so 
	# this is necessary
	
	a + b
}
increment <- mcPartial(plus, a = 1)

Map("increment", 1:10)

## find every email in a vector

is_email <- mcPartial(grepl, pattern = "[^@]+[@][^.]+[.].+")

mcSelect(is_email, c("hi@gmail.com", "cran@cran.cran", "not email"))
