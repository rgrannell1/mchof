
h <- function (name) name == "cat"
w <- function (name) name == "dog"

cat_or_dog <- mcXor(h, w)
cat_or_dog("rat")

# example without saving result of mcXor to 
# intermediate function

f <- function (n) n > 5
g <- function (n) n < 10

mcXor(f, g)(7)

## pairing mcXor with mcSelect

mcSelect(
	mcXor(
		function (x) x < 10,
		function (x) x == 7),
	seq_len(20)
)
