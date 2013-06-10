
context ("test that windows-specific features work")

test_that("as of 0.3 a message is displayed", {

	if (.Platform$OS.type == 'windows') {
		expect_message(
			call_mclapply(identity, 1:10, list(mc.cores = 1)), 'windows')
	}
})