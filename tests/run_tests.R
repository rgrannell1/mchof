
library(mchof)
library(itertools)
library(testthat)

options(forall_time = 0.5)
	
	test_package("mchof")

options(forall_time = NULL)

ISSUE("need to avoid contamination")
ISSUE("fix zip")
ISSUE("swap out match.call() everywhere!")