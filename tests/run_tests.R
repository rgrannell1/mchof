
library(mchof)
library(itertools)
library(testthat)

options(forall_time = 2)
	
	test_package("mchof")

options(forall_time = NULL)

ISSUE("do functions pick up arguments from where they're created accidentally?")
