
benchmark <- benchmark_exp <- list()

if ( Sys.info()["user"] == "rgrannell1") {
	# save CRAN the horror of running such 
	# expensive benchmarks

	require(microbenchmark)
	options(mc.cores = NULL)

	collated_timings <- list()

	# using a fixed path makes it hard from others to run,
	# but I'm likely the only one who will want to run this code

	path <- "/home/rgrannell1/Dropbox/R directory/mchof/inst/benchmark"
	all_tests <- paste0(path, "/", list.files(
		path, 
		"^benchmark[-][^-]+[^.]+[.][rR]"
	))

	source(paste0(path, "/utils-benchmark.r"))
	sapply(all_tests, function (file) source(file))

	len <- 10000
	times <- 100

	for (test in sample(names(benchmark))) {

		timings <- benchmark_code( benchmark[[test]], len, times )
		collated_timings[[test]] <- timings
		visualise_benchmark(timings)	
	}
	collated_timings
}
