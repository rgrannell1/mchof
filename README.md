mchof - MultiCore Higher-Order Functions - v0.3.1
===

[![Build Status](https://travis-ci.org/rgrannell1/mchof.png)](https://travis-ci.org/rgrannell1/mchof)

Mchof (read: mac'hoff) is a utility library that mixes two good things: 
higher-order functions and parallelism. It allows functional programs to benefit 
for a large speedup. This is done with little configuration; the user simply needs to 
specify how many cores you'd like to use.

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

