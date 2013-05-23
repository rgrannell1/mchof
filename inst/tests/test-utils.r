
context ("util: check that forall works")

	test_that("basic cases don't throw errors", {
		
		forall(
			list(x = 1:10, y = 1:10),
			function (x, y) x + y == y + x,
			info = 'integer addition commutativity')
		
		forall(
			list(x = 1:10, y = 1:10, z = 1:10),
			function (x, y, z) (x + y) + z == x + (y + z),
			info = 'integer addition associativity')
		
		forall(
			list(x = c('', 'a', 'b', 'aa'), y = c('', 'abc', 'abcd', 'a')),
			function (x, y) nchar(paste0(x, y)) > max(nchar(x), nchar(y)),
			function (x, y) nchar(x) > 0 && nchar(y) > 0,
			info = 'string concat. yields a longer string')

	})
