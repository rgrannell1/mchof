
library(mchof)
library(itertools)
library(testthat)

# test utilities
source('/home/rgrannell1/Dropbox/R directory/mchof/R/utils.R')
source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_utils.R')
source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_generators.R')

options(forall_time = 200)
test_dir('/home/rgrannell1/Dropbox/R directory/mchof/inst/tests/', rep = 'summary')

options(forall_time = NULL)
