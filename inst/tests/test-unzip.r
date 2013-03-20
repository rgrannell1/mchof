
context ("test that mcUnzip is mostly inverse to mcZip...mostly")

test_that ("verify that is a rough inverse of mcZip", {
		
	zip_identity <- function (..., paropts) {		
		mcUnzip (mcZip (..., paropts))
	}
	
	expect_equal (
		zip_identity (
			list (1, 2, 3),	
			list (4, 5, 6) ),	
		list (
			list (1, 2, 3),	
			list (4, 5, 6) ) )
	
	expect_equal(
		zip_identity (
			
			
			
		),
		
	)
	
})

test_that ("normal testing", {
	
	
	
})
