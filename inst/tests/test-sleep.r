
context("sleep: normal cases")

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
