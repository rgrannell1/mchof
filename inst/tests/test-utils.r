
context ("check that forall works (Quis custodiet ipsos custodes?)")

	test_that("simple laws hold when tested with forall", {
		
		forall(
			info = "integer addition is commutative",
			list(x = 1:10, y = 1:10),
			function (x, y) x + y == y + x
		)
		
		forall(
			list(x = 1:10, y = 1:10, z = 1:10),
			function (x, y, z) (x + y) + z == x + (y + z),
			info = 'integer addition is associative')
		
		forall(
			info = "string concatenation yields a longer string",
			list(x = c('', 'a', 'b', 'aa'), y = c('', 'abc', 'abcd', 'a')),
			function (x, y) nchar(paste0(x, y)) > max(nchar(x), nchar(y)),
			given = function (x, y) nchar(x) > 0 && nchar(y) > 0
		)
	})
