mchof - MultiCore Higher-Order Functions
===

`mchof`	 provides convenient, consistent parallel implementations of several 
commonly used higher-order functions found in base R. Excellent parallel 
implementations of map-like functions exist in packages such as `plyr` and
`multicore`, giving users better efficiency with little work on their part. 
This package aims to extend this speedup to people who use functions like 
`Filter` or `Reduce` in their code.

- Special cases handled consistently between functions.
- Built on the user-friendly `multicore` parallel library; all the user needs to do is 
 specify how many cores they would like to use to get 90% of the benefits of 
 running in parallel. 
