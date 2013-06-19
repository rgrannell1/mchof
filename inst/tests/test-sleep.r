
context("mcSleep: normal cases")

expect_that(
	mcSleep(mean, 0.2)(1),
	takes_less_than(0.3)
)

forall(info = "mcSleeps doesn't alter the value of f",
	list(x_ = r_integers()),
	function (x_) {
		mcSleep( function (x) x * x, 0)(x_) == x_ * x_
	}
)

context("mcTimer: normal cases")

test_that({

	func <- mcTimer(0.5)
	first <- func()

	Sys.sleep(1)

	func <- mcTimer(0.5)
	second <- func()

	expect_true(first)
	expect_false(second)
	
})