
context ("test that mcZipWith is well behaves")

test_that("test structure and values", {
	
	expect_equal(
		mcZipWith (
			function (p) paste0(p, collapse = ''),
			list( list('a', 'b'), list('d', 'e', 'f') ) ),	
		list (
			c ("ad"),	
			c ("be")
		) )	
	
	expect_equal(
		mcZipWith (
			function (v) list(paste0(v, collapse = '')),
			list( list('a', 'b'), list('d', 'e', 'f') ) ),	
		list (
			list ("ad"),	
			list ("be")
		) )	
	
	expect_equal(info='',
		mcZipWith (
			identity,
			list(
				list( list(), list() ), 
				list( list(), list() ),
				list( list(), list() ))),	
		list(
			list( list(), list(), list() ), 
			list( list(), list(), list() ) )
		)
})


context ("test that mcZip is well behaved for normal cases")

test_that("normal cases", {
		
	expect_equal(
		mcZip (list( list('a', 'b'), list('d', 'e', 'f'))),	
		list (
			list ('a', 'd'),	
			list ('b', 'e')
		) )
	
	expect_equal(
		mcZip (list(1:3, 4:6), paropts = list (mc.cores = 2)),	
			list (
				list(1,4),
				list(2,5),
				list(3,6)
			) )
	
	expect_equal(
		mcZip (
			list( list (matrix(1:4, 2,2)), list (matrix(2:5, 2,2))),
			paropts = list (mc.cores = 2)),	
		list (
			list(matrix(1:4, 2,2), matrix(2:5,2,2))
		) )
	
	expect_equal(
		mcZip (
			list(
				list( list(), list() ), 
				list( list(), list() ))
			),	
		list(
			list(list(), list()),
			list(list(), list())) )
})