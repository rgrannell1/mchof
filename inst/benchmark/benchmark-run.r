
options(mc.cores = NULL)

source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_utils.R')
source("/home/rgrannell1/Dropbox/R directory/mchof/inst/benchmark/benchmark-mchof.r")

visualise_benchmark <- function (data) {
	
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
		g <- ggplot(data = molten[[i]], aes(x = value)) + 
			geom_density(aes(group = variable, fill = variable), alpha = 0.3) + 
			xlab("nanoseconds") + ggtitle(paste0(names(molten[i]), " Performance"))
		plot(g)
		Sys.sleep(3)
	}

}

backend_data <- benchmark_code(backend_tests, len = 10000, 300)
mchof_data <- benchmark_code(mchof_tests, len = 10000, 300)

visualise_benchmark(mchof_data)


