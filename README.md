mchof - MultiCore Higher-Order Functions
===
**Version 0.3**

mchof (read: mac'hoff) is a utility library that provides
composable, consistent parallel implementations of functionals such as
Filter, Fold, ZipWith and Partition.

 * Special cases handled consistently between functions to allow composabililty.   
 * Built on the user-friendly multicore parallel library; all the user needs to 
   do is specify how many cores they would like to use to get 90% of the benefits 
   of running in parallel.

This library was written out of frustration that certain 'embarassingly parallel' functions like filter
had no parallel implementations in R, and that the existing single core implementations had inconsistent corner
cases.

###Functions included in the current release (0.3)

####Filtering a list based on a predicate:

* **mcFilter/mcSelect**: get the elements of a list matching a predicate
* **mcPartition:** split a list into values that return true or false for predicate
* **mcReject**: get the elements of a list not matching a predicate

####Finding elements in a list:

* **mcFind:** get the first element in a list matching a predicate
* **mcPosition:** find the first element in a list matching a predicate

####Reducing a function over a list:

* **mcReduce:** applies an associative binary operator to a list (eg. +, *, rbind)
* **mcFold**: applies an associative binary operator to a list, with an initial value

####Zipping and unzip lists:

* **mcZipWith:** combine n lists into a list of n-tuples, apply a function to result
* **mcZip:** combine n lists into a list of n-tuples
* **mcUnzip:** split a list of n-tuples into n lists
* **mcUnzipWith:** split a list of n-tuples into n lists, apply a function to the results

####Quantifying true/cases for a predicate:

* **mcAny**: check if a predicate is true for at least one element in a list
* **mcAll**: check if a predicate is true for all elements in a list
* **mcOne**: check if a predicate is true for one element in a list

