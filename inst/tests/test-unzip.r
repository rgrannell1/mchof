
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
	
	expect_equal(info = 'trimming works correctly',
		cancelled(list(
			list('c', 'b', 'a'),
			list(1, 2, 3, 4)
		)),	
		list (
			list('c', 'b', 'a'),
			list(1, 2, 3)
	))
	
	expect_equal(
		cancelled(
			list (
				list( list(), list(), list() ),	
				list( list(), list(), list() ))	 	
		), 	
		list (
			list( list(), list(), list() ),	
			list( list(), list(), list() ))
	)

})

test_that ("normal testing of unzip", {
	
	expect_equal(
		mcUnzip(
			list(
				list('a', 1),
				list('b', 2, 3)
			)),
		list(
			list("a", "b"),
			list(1, 2)))
	
	expect_equal(
		mcUnzip(
			list()	
		),	
		list () )
	
	expect_equal(
		mcUnzip(
			list(
				c('cat', 'ticks'),
				c('dog', 'rabies'))
		),
		list(
			list('cat', 'dog'),
			list('ticks', 'rabies')
	))	

})

test_that("mcUnzipWith", {
	
	expect_equal(
		mcUnzipWith(
			function (li) {
				lapply (li, paste)
			},
			list (
				list ('ape', 1),
				list ('dog', 2),
				list ('monkey', 3)
			),
			paropts = list(mc.cores = 3)
		),	
		list (
			list ('ape', 'dog', 'monkey'),
			list ('1', '2', '3')
	))	
})
