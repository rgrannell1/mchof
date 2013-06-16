

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
