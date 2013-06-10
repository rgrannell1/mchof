
h <- function (name) name == "cat"
w <- function (name) name == "dog"

cat_or_dog <- mcOr(h, w)
cat_or_dog("rat")

# example without saving result of mcOr to 
# intermediate function

f <- function (n) n > 5
g <- function (n) n < 10

mcOr(f, g)(7)

## pairing mcOr with mcSelect

mcSelect(
	mcOr(
		function (x) x < 10,
		function (x) x == 7),
	seq_len(20)
)
