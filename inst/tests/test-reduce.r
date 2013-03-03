
context("test that mcReduce works consistently")

test_that(
	"odd-numbered cases work", {
	
	expect_equal(
		mcReduce(
			f = function (x, y) {
				paste0(x, y)
			},	
			x = list('a', 'b', 'c', 'd', 'e', 'f', 'g'),
			paropts = list(mc.cores = 1)),
		"abcdefg")
	
	# too many cores:
	expect_equal(
		mcReduce(
			f = function (x, y) x + y,	
			x = 1:9,
			paropts = list(mc.cores = 20)),
		45)
	
	expect_equal(
		mcReduce(
			f = function (x, y) {
				c(x,y)
			},	
			x = list(list(a='a'), list(b='b'), list(c='c')),
			paropts = list(mc.cores = 3)),
		list(a = 'a', b = 'b', c = 'c'))
	
})

test_that(
	"even cases work", {
	
	expect_equal(
		mcReduce(
			f = function (x, y) {
				c(x,y)
			},	
			x = list(list(a='a'), list(b='b'), list(c='c'), list(d='d')),
			paropts = list(mc.cores = 5)),		
		list(a = 'a', b = 'b', c = 'c', d = 'd'))
	
	expect_equal(
		mcReduce(
			f = function (x, y) {
				x + y
			},	
			x = 1:10,
			paropts = list(mc.cores = 12)), 55)
	
})
