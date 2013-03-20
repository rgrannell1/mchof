
context ("test that mcUnzip is mostly inverse to mcZip...mostly")

test_that ("test that inverse works", {
		
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
	
})

test_that ("", {
	
	
	
	
})
