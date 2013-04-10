mchof - MultiCore Higher-Order Functions
===

mchof (read: mc'hoff) provides convenient, consistent parallel implementations of several 
commonly used higher-order functions found in base R. Excellent parallel 
implementations of map-like functions exist in packages such as `plyr` and
`multicore`, giving users better efficiency with little work on their part. 
This package aims to extend this speedup to people who use functions like 
Filter or Reduce in their code.

 * Special cases handled consistently with functions in base R.	
 * Built on the user-friendly `multicore` parallel library; all the user needs to 
   do is specify how many cores they would like to use to get 90% of the benefits 
   of running in parallel
   
Functions included in the current release

* **mcFilter**: get the elements of a list matching a predicate
* **mcReduce:** applies an associative binary operator to a list (eg. +, *, rbind)
* **mcFind:** get the first element in a list matching a predicate
* **mcPosition:** find the first element in a list matching a predicate

Functions in development (April 30th < ETA < May 15th 2013)

* **mcZipWith:** combine n lists into a list of n-tuples, apply a function to result
* **mcZip:** combine n lists into a list of n-tuples
* **mcUnzip:** split a list of n-tuples into n lists
* **mcUnzipWith:** split a list of n-tuples into n lists, apply a function to the results
* **mcPartition:** split a list into values that return TRUE and FALSE
