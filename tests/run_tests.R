
library(mchof)
library(itertools)
library(testthat)

options(forall_time = 0.2)
	
	test_package("mchof")

options(forall_time = NULL)

ISSUE("need to avoid contamination")
ISSUE("check blank examples")
ISSUE("check no deleted docs included")
ISSUE("fix partial")
ISSUE("add type-checking to math and logical functions")
ISSUE("add families to all functions")
ISSUE("reduce boilerplate in math")

# pluck list() and A(0)
