
library(mchof)
library(itertools)
library(testthat)

source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_utils.R')
source('/home/rgrannell1/Dropbox/R directory/mchof/tests/test_generators.R')

FLAG("need to set global time option for forall, to respect CRAN")

options(forall_time = .2)

test_dir('/home/rgrannell1/Dropbox/R directory/mchof/inst/tests/', rep = 'summary')
