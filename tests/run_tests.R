
library(mchof)
library(itertools)
library(testthat)

options(forall_time = 0.2)
	
test_package("mchof")

options(forall_time = NULL)
 
warning("need to avoid contamination")
warning("move to require a testing")