
library(mchof)
library(itertools)
library(testthat)

options(forall_time = 0.2)
	
	test_package("mchof")

options(forall_time = NULL)

#ISSUE("read and fix all docs")
#ISSUE("check that mcAll doesn't throw factor errors")
