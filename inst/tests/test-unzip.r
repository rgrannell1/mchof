
context ("test that mcUnzip is mostly inverse to mcZip...mostly")

test_that ("verify that is a rough inverse of mcZip", {
	
	'%of%' <- function (f, g) {
		function (...) f(g(...))
	}
	cancelled <- mcUnzip %of% mcZip
	
	expect_equal(
		cancelled( list(
			list ('a', 'b', 'c'),
			list(1, 2, 3)
		)),
	list (
		list('a', 'b', 'c'),
		list(1, 2, 3)
	))
	
	# trimming works correctly
	expect_equal(
		cancelled(list(
			list('c', 'b', 'a'),
			list(1, 2, 3, 4)
		)),	
		list (
			list('c', 'b', 'a'),
			list(1, 2, 3)
	))
	
})

test_that ("normal testing", {
	
	
	
})
