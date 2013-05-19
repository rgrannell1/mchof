
context("check that Find's normal cases work")

test_that("variety of cases", {
	
	expect_equal(
		mcFind(
			f = function (x){
				x == 'c'
			},	
			x = c('a', 'b', 'c', 'd'),
			right = FALSE,
			paropts = list(mc.cores = 2) ), 'c')
	
	expect_equal(
		mcFind(
			f = function (x) x == 'c',	
			x = c('a', 'b', 'c', 'd'),
			right = TRUE,
			paropts = list(mc.cores = 10) ), 'c')
	
	expect_equal(
		mcFind(
			f = function (x) x > 5,	
			x = 1:10,
			right = TRUE,
			paropts = list(mc.cores = 20) ), 10)
	
	expect_equal(
		mcFind(
			f = function (x) x > 5 && x < 8,	
			x = 1:10,
			right = FALSE,
			paropts = list(mc.cores = 5) ), 6)
	
	expect_equal(
		mcFind(
			f = function (x) x > 5 && x < 8,	
			x = 1:10,
			right = TRUE,
			paropts = list(mc.cores = 5) ), 7)

})




