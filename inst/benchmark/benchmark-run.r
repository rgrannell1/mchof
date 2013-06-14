	
visualise_benchmark <- function (data) {

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
		g <- ggplot(data = molten[[i]], aes(x = value)) + 
			geom_density(aes(group = variable, fill = variable)) + 
			xlab("nanoseconds") + ggtitle(paste0(names(molten[i]), " vs control"))
		plot(g)
		Sys.sleep(9)
	}
}

DONTRUN({
	options(mc.cores = NULL)
	require(reshape2)
	require(ggplot2)
	
	backend_data <- benchmark_code(backend_tests, len = 1000, 400)
	iterate_data <- benchmark_code(iterate_tests, len = 1000, 400)
	group_into_data <-benchmark_code(group_into_tests, 1000, 400)
	
	mchof_data <- benchmark_code(mchof_tests, len = 1000, 400)
	visualise_benchmark(mchof_data)	
})
