
context("check that assertion-based checking actually works")


test_that('exception-free axioms should hold', {
	
	assert (
		'addition is commutative', 
		function (a, b) a + b == b + a,
		where = list(
			a = seq_len(100),
			b = seq_len(100))
	)
	
	assert (
		'multiplication is commutative', 
		function (a, b) a * b == b * a,
		where = list(
			a = seq_len(100),
			b = seq_len(100))
	)
	
})

test_that('axioms with stated exceptions hold', {
	
	assert (
		'length List a * List b > length List a || List b, unless length is 0',	
		function (a, b) {
			length (c(a, b)) > max(length(a), length(b))
		},
		where = list(
			a = List(100),	
			b = List(100)),
		unless = function (a, b) {
			length(a) == 0 || length(b) == 0
		})	
})
