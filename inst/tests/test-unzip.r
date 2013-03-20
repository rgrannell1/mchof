
context ("test that mcUnzip is mostly inverse to mcZip")

test_that ("", {
		
	zip_identity <- function (...) {
		
		mcUnzip (mcZip (...))
		
	}
	
	expect_equal (
		zip_identity (
			list (1, 2, 3),	
			list (4, 5, 6) ),	
		list (
			list (1, 2, 3),	
			list (4, 5, 6) )
		
	)
	
	
	
})
