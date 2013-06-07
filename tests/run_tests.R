
library(mchof)
library(itertools)
library(testthat)

# test utilities
source('/home/rgrannell1/Dropbox/R directory/mchof/R/utils.R')
source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_utils.R')
source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_generators.R')

ISSUE("fix mchof.Rs code tags")
ISSUE("add tests to zip")
ISSUE("improve len-x.r")
ISSUE("point to website")
ISSUE("special cases need to be nailed down for 0.3")

options(forall_time = 2)
test_dir('/home/rgrannell1/Dropbox/R directory/mchof/inst/tests/', rep = 'summary')

options(forall_time = NULL)
