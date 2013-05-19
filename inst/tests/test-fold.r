
context("test that mcFold works consistently")

test_that(
	"even-numbered cases work", {
		
		expect_equal(
			mcFold(
				f = function (x, y) {
					paste0(x, y)
				},
				first = 'a',
				x = list('b', 'c', 'd', 'e', 'f', 'g', 'h'),
				paropts = list(mc.cores = 1)),
			"abcdefgh")
		
		# too many cores:
		expect_equal(
			mcFold(
				f = function (x, y) x + y,
				first = 0,
				x = 1:9,
				paropts = list(mc.cores = 20)),
			45)
		
		expect_equal(
			mcFold(
				f = function (x, y) c(x,y),
				first = NULL,
				x = list(list(a='a'), list(b='b'), list(c='c')),
				paropts = list(mc.cores = 3)),
			list(a = 'a', b = 'b', c = 'c'))
		
	})

test_that(
	"odd cases work", {
		
		expect_equal(
			mcFold(
				f = function (x, y) {
					c(x,y)
				},
				first = NULL,
				x = list(list(a='a'), list(b='b'), list(c='c'), list(d='d')),
				paropts = list(mc.cores = 5)),		
			list(a = 'a', b = 'b', c = 'c', d = 'd'))
		
		expect_equal(
			mcFold(
				f = function (x, y) {
					x + y
				},	
				first = 0,
				x = 1:10,
				paropts = list(mc.cores = 12)), 55)
		
	})
