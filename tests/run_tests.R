
library(mchof)
library(itertools)
library(testthat)

# test utilities
source('/home/rgrannell1/Dropbox/R directory/mchof/R/utils.R')
source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_utils.R')
source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_generators.R')

ISSUE("read and fix all docs")
ISSUE("add fold examples")
ISSUE("docs are lying for mcreduce and mcfold")
ISSUE("remove comments in test-fold")

ISSUE("fix path references!!! Cant be tested on server atm")

options(forall_time = 0.2)
test_dir('/home/rgrannell1/Dropbox/R directory/mchof/inst/tests/', rep = 'summary')

options(forall_time = NULL)
