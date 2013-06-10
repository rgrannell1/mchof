
h <- function (name) name == "cat"
w <- function (name) name == "dog"

cat_or_dog <- mcAnd(h, w)
cat_or_dog("rat")

# example without saving result of mcAnd to 
# intermediate function

f <- function (n) n > 5
g <- function (n) n < 10

mcAnd(f, g)(7)

## pairing mcAnd with mcSelect

mcSelect(
	mcAnd(
		function (x) x < 10,
		function (x) x == 7),
	seq_len(20)
)
