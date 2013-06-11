
library(mchof)
library(itertools)
library(testthat)

options(forall_time = 2)
	
	test_package("mchof")

options(forall_time = NULL)

ISSUE("need to add local() to all functions, to avoid contamination. do in a branch")