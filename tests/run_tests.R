
library(mchof)
library(itertools)
library(testthat)

options(forall_time = 0.2)
	
	test_package("mchof")

options(forall_time = NULL)
 
warning("need to avoid contamination")
warning("check blank examples")
warning("check no deleted docs included")
warning("fix partial")
warning("add families to all functions")
warning("fix mcOne")
warning("make mcParameters(prim, x) work")

warning("use  as.list(parse(text=''))  to solve prob with formals")
warning("juxtapose ((juxt a b c) x) => [(a x) (b x) (c x)]")
