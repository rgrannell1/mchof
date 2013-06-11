
context("logic: normal cases")

na <- function (...) NA
true <- function (...) TRUE
false <- function (...) FALSE

expect_na <- function (object) {
	expect_equal(object, NA)
}

test_that("mcAnd truth tables", {
	
	expect_true( mcAnd(true, true)() )
	expect_false( mcAnd(true, false)() )
	expect_false( mcAnd(false, true)() )
	expect_false( mcAnd(false, false)() )
	expect_na( mcAnd(na, na)() )
	expect_na( mcAnd(na, true)() )
	expect_na( mcAnd(true, na)() )
	expect_false( mcAnd(na, false)() )
	expect_false( mcAnd(false, na)() )	
})


test_that("mcNot truth tables", {
	expect_false( mcNot(true)() )
	expect_true( mcNot(false)() )
	expect_na( mcNot(na)() )	
})


test_that("mcOr truth tables", {
	expect_true( mcOr(true, true)() )
	expect_true( mcOr(true, false)() )
	expect_true( mcOr(false, true)() )
	expect_false( mcOr(false, false)() )
	expect_na( mcOr(na, na)() )
	expect_true( mcOr(na, true)() )
	expect_true( mcOr(true, na)() )
	expect_na( mcOr(na, false)() )
	expect_na( mcOr(false, na)() )	
})

test_that("mcXor truth tables", {
	expect_false( mcXor(true, true)() )
	expect_true( mcXor(true, false)() )
	expect_true( mcXor(false, true)() )
	expect_false( mcXor(false, false)() )
	expect_na( mcXor(na, na)() )
	expect_na( mcXor(na, true)() )
	expect_na( mcXor(true, na)() )
	expect_na( mcXor(na, false)() )
	expect_na( mcXor(false, na)() )
})

