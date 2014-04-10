mchof - MultiCore Higher-Order Functions - v0.3.1
===

[![Build Status](https://travis-ci.org/rgrannell1/mchof.png)](https://travis-ci.org/rgrannell1/mchof)

Mchof (read: mac'hoff) is a utility library that mixes two good things: 
higher-order functions and parallelism.


### Filtering a list based on a predicate:

* **mcFilter/mcSelect**: get the elements of a list matching a predicate
* **mcPartition:** split a list into values that return true or false for predicate
* **mcReject**: get the elements of a list not matching a predicate

### Finding elements in a list:

* **mcFind:** get the first element in a list matching a predicate
* **mcPosition:** find the first element in a list matching a predicate

### Reducing a function over a list:

* **mcReduce:** applies an associative binary operator to a list (eg. +, *, rbind)
* **mcFold**: applies an associative binary operator to a list, with an initial value

### Zipping and unzip lists:

* **mcZipWith:** combine n lists into a list of n-tuples, apply a function to result
* **mcZip:** combine n lists into a list of n-tuples
* **mcUnzip:** split a list of n-tuples into n lists
* **mcUnzipWith:** split a list of n-tuples into n lists, apply a function to the results

### Quantifying true/cases for a predicate:

* **mcAny**: check if a predicate is true for at least one element in a list
* **mcAll**: check if a predicate is true for all elements in a list
* **mcOne**: check if a predicate is true for one element in a list

## Authors

Ryan Grannell

## Licensing

Mchof is released under the GPL-2.

## NOTE

Mchof is currently feature complete and is in maintainance mode, for the following reasons.

- **Poor Constant Time Efficiencies:** only unusually slow functions (or large collections)
actually benefit from being parallelised via process forking. Less of my programs actually benefited from 
using mclapply than expected.

- **Memory Overconsuption:** in the cases where programs do benefit from process forking I've found
that mclapply - the function mchof is based around - often swallows all system memory, forcing a 
hard reset of the computer. This is very frustrating.

- **Error Reporting:** it is difficult to get clear error messages back from mclapply, which makes
using the function deeply unpleasant.

- **Rcpp:** Rcpp has become the de facto tool for high-performance computing in R, with parallel taking 
a backseat. 

Mchof is still available, and still offers at least as much benefits as its parent library 'parallel'. 
Use it at your leisure.

It's been fun,
Ryan.
