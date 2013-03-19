
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
			list ('a', 'd'),	
			list ('b', 'e')
		) )
	
	expect_equal(
		mcZip (1:3, 4:6, paropts = list (mc.cores = 2)),	
			list (
				list(1,4),
				list(2,5),
				list(3,6)
			) )
	
	expect_equal(
		mcZip (c(a = 1, b = 2), c(c = 4, d = 3), paropts = list (mc.cores = 2)),	
		list (
			list(a=1,c=4),
			list(b=2,d=3)
		) )
	
})