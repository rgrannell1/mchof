
options(mc.cores = NULL)

source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_utils.R')
source("/home/rgrannell1/Dropbox/R directory/mchof/inst/benchmark/benchmark-mchof.r")
source("/home/rgrannell1/Dropbox/R directory/mchof/inst/benchmark/benchmark-experiments.r")

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
		Sys.sleep(4)
	}

}
len <- 300000
times <- 300

backend_data <- benchmark_code(backend_tests, len, times)
dual_core_data <- benchmark_code(dual_core_backend_tests, len, times)
# natural intersection of dual-core and lapply performance at 

mchof_data <- benchmark_code(mchof_tests, len, times)

iterate_data <- benchmark_code(iterate_tests, len, times)
group_into_data <- benchmark_code(group_into_tests, len, times)

visualise_benchmark(backend_data)
visualise_benchmark(dual_core_data)

visualise_benchmark(iterate_data)
visualise_benchmark(group_into_data)


