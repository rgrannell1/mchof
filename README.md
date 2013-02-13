mchof - MultiCore Higher-Order Functions
===

mchof provides convenient, consistent parallel implementations of several 
commonly used higher-order functions found in base R. Excellent parallel 
implementations of map-like functions exist in packages such as ```plyr and
```multicore```, giving users better efficiency with little work on their part. 
This package aims to further improve the efficiency of code written in a functional
style.

- Special cases handled consistently between functions
- Built on the simple `multicore` parallel library; all the user needs to do is 
- specify how many cores they would like to use.

