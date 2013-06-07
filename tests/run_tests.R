
library(mchof)
library(itertools)
library(testthat)

source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_utils.R')
source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_generators.R')
source('/home/rgrannell1/Dropbox/R directory/mchof/R/utils.R')

options(forall_time = 0.2)

test_dir('/home/rgrannell1/Dropbox/R directory/mchof/inst/tests/', rep = 'summary')

options(forall_time = NULL)
