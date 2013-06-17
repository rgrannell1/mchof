
benchmark <- benchmark_exp <- list()

if ( Sys.info()["user"] == "rgrannell1") {
	# save CRAN the horror of running such 
	# expensive benchmarks

	require(microbenchmark)
	options(mc.cores = NULL)

	collated_timings <- list()

	# using a fixed path makes it hard from others to run,
	# but I'm likely the only one who will want to run this code

	which_tests <- 2

	suffix_regex <- c(
		"^benchmark[-][^-]+[^.]+[.][rR]",
		"^benchmark[-]exp.+[.][rR]")

	path <- "/home/rgrannell1/Dropbox/R directory/mchof/inst/benchmark"
	all_tests <- paste0(path, "/", list.files(
		path, 
		suffix_regex[[which_tests]]
	))

	source(paste0(path, "/utils-benchmark.r"))
	sapply(all_tests, function (file) source(file))


	benchmark_obj <- list(
		benchmark,
		benchmark_exp
	)[[which_tests]]

	len <- 10000
	times <- 100

	for (test in sample(names(benchmark_obj))) {

		timings <- benchmark_code( benchmark_obj[[test]], len, times )
		collated_timings[[test]] <- timings
		visualise_benchmark(timings)	
	}
	invisible( collated_timings )
}
