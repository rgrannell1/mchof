
context ("test that every function can take a function name")

truefun <- function (x) TRUE

test_that ("", {
	
	expect_equal (
		mcFilter ("truefun", 1:10),	
		1:10 )
})

test_that ("", {
	
	expect_equal (
		mcPosition (
			"truefun",	
			c('a', 'b', 'c') ),		
		1 )	
})

test_that ("", {
	
	expect_equal (
		mcReduce ("+", 1:10),
		55 )
	
})

test_that ("", {
	
	expect_equal(
		mcFind (
			truefun,	
			letters[1:10] ),
		'a')
	
})

test_that ("", {
	
	expect_equal (
		mcPartition (
			"truefun",	
			1:4
		),		
		list (
			1:4,	
			integer(0)
		) )
	
})


test_that ("", {
	
	expect_equal (
		mcZipWith (
			'identity', 
			list(
				c(1:3),	   
				c(4:6)) 
		),
		list (
			list(1,4),	
			list(2,5),
			list(3,6)) )
	
})

test_that ("", {
	
	expect_equal (
		mcUnzipWith (
			'identity', 
			list (
				list(1,4),	
				list(2,5),
				list(3,6))
		),
		list(
			list(1, 2, 3),	   
			list(4, 5, 6))  )
	
})