
benchmark_code <- function (tests, len = 100, times = 2) {
	# compare the run-times of tests to controls, 
	# for the sequence 1...len

	report_result <- function (name, multiplier) {
		multiplier <- round(multiplier, 2)	

		messagef(
			"%s was %s times slower than the control test",
			name, paste(multiplier[1], "+-", multiplier[2]))
	}
	compare_results <- function (test, control) {
		# calculate how many times larger x is than y,
		# within a 95% confidence interval
		
		difference <-  median(test) / median(control)
		margin <- abs(
			( median(test) - 2*sd(test) / median(control) + 2*sd(control) ) /
			( median(test) + 2*sd(test) / median(control) - 2*sd(control) ))
			
		c(difference, margin)
	}
	
	timing <- Map(
		function (test) {
			
			cat("..")
			
			tryCatch(
				list(
					name = test$name,
					test = microbenchmark(
						test$test( seq_len(len) ),
						times = times)$time,
					control = microbenchmark(
						test$control( seq_len(len) ),
						times = times)$time),
				error = function (error) {
					stopf(c("%s", "%s"), test$name, error)
				}
			)

		},
		tests)
	cat("\n")
	Map(
		function (test) {
			report_result(
				name = test$name,
				multiplier = compare_results(test$test, test$control))
		},
		timing)
	
	invisible(timing)
}
	
visualise_benchmark <- function (data) {
	# plot the distribution of results for the control
	# and test times, to get an idea of how efficient the 
	# new function is

	require(reshape2)
	require(ggplot2)

	molten <- structure(Map(
		function(x) {
			melt(data.frame(
				name = x$name,
				test = x$test,
				control = x$control), id.vars = "name")
		},
		data),
		names = sapply(data, function (li) li$name))
	
	for (i in seq_along(molten)) {

		g <- ggplot(data = molten[[i]], aes(x = value / 1000)) + 
			geom_density(aes(group = variable, color = variable),
				alpha = 0.3) + 
			xlab("microseconds") + ggtitle(paste0(names(molten[i]), " vs control"))

		plot(g)
		Sys.sleep(6)
	}
}

