
library(mchof)
library(itertools)
library(testthat)

options(forall_time = 0.21)
	
	test_package("mchof")

options(forall_time = NULL)

ISSUE("need to avoid contamination")
ISSUE("check blank examples")
ISSUE("try to ensure all formals are preserved;")

ISSUE("refactor function so they work regardless of func formals")
ISSUE("need to fix enviromnent of mcLarger")
ISSUE("think can fix functional problem; if formals can be added make different 
	function rather than add arguments manuall")

# pluck list() and A(0)