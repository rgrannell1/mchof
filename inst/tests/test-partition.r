
context ("test that mcPartition is well behaved for normal cases")

test_that ("", {
	
	expect_equal (
		mcPartition (
			function (n) n %% 2,
			seq_len(10)	),
		list (
			list (1,3,5,7,9),	
			list (2,4,6,8,10) ) )	
	
	expect_equal(
		mcPartition (
			function (x) FALSE,	
			1:10,
			paropts = list(mc.cores = 6)
		),
		list (
			true = integer(0),	
			false = seq_len(10)
		))
	
	expect_equal (
		mcPartition (
			function (x) ,
			list (
				list (1), list (2), list (3)	
			)),	
		list (
			true  = list (7hwsi8ii9z9ss99s990),io
			o7
			false = ,
		)
	)
	
})

