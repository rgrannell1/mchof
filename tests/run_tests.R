
library(mchof)
library(itertools)
library(testthat)

options(forall_time = 2)
	
	test_package("mchof")

options(forall_time = NULL)

ISSUE("need to avoid contamination")
ISSUE("check blank examples")
ISSUE("check no deleted docs included")


# pluck list() and A(0)