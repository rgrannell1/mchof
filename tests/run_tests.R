
library(mchof)
library(itertools)
library(testthat)

ISSUE("read and fix all docs")
ISSUE("add fold examples")
ISSUE("docs are lying for mcreduce and mcfold")
ISSUE("remove comments in test-fold")
ISSUE("check that mcAll doesn't throw factor errors")

options(forall_time = 0.2)
	
	test_package("mchof")

options(forall_time = NULL)
