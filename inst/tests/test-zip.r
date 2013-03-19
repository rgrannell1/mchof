
context ("test that mcZip is well behaved for normal cases")

test_that("", {
	
	expect_equal(
		mcZip (c(1, 2, 3), c(2, 3, 4)),
		list (
			list (1:2), 
			list (2:3),
			list (3:4) ) )
	
	expect_equal(
		mcZip (list('a', 'b'), list('d', 'e', 'f')),	
		list (
			list ('a', 'b'),	
			list ('d', 'e')
		)
	)
	
})