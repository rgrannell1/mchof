
library(mchof)
library(itertools)
library(testthat)

options(forall_time = 1)
	
	test_package("mchof")

options(forall_time = NULL)

ISSUE("need to avoid contamination")
ISSUE("Move test-pluck")
ISSUE("check blank examples")
