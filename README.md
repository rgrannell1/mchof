mchof - MultiCore Higher-Order Functions
===
Version 0.3

mchof (read: mac'hoff) is a utility library that provides
composable, consistent parallel implementations of functionals such as
Filter, Fold, ZipWith and Partition.

 * Special cases handled consistently between functions to allow composabililty.	
 * Built on the user-friendly multicore parallel library; all the user needs to 
   do is specify how many cores they would like to use to get 90% of the benefits 
   of running in parallel

Functions included in the current release (0.3)

* **mcFilter/mcSelect**: get the elements of a list matching a predicate
* **mcReduce:** applies an associative binary operator to a list (eg. +, *, rbind)
* **mcFind:** get the first element in a list matching a predicate
* **mcPosition:** find the first element in a list matching a predicate
* **mcZipWith:** combine n lists into a list of n-tuples, apply a function to result
* **mcZip:** combine n lists into a list of n-tuples
* **mcUnzip:** split a list of n-tuples into n lists
* **mcUnzipWith:** split a list of n-tuples into n lists, apply a function to the results
* **mcPartition:** split a list into values that return TRUE and FALSE
* **mcAny**: check if a predicate is true for at least one element in a list
* **mcAll**: check if a predicate is true for all elements in a list
* **mcOne**: check if a predicate is true for one element in a list
* **mcFold**: applies an associative binary operator to a list, with an initial value
* **mcReject**: get the elements of a list not matching a predicate

Functions in development (0.3) eta July 15th - August 1st

