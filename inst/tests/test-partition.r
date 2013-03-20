
context ("test that mcPartition is well behaved for normal cases")

test_that ("", {
	
	expect_equal (
		mcPartition (
			function (n) n %% 2,
			seq_len(10)	),
		list (
			list (1,3,5,7,9),	
			list (2,4,6,8,10) ) )	
	
})