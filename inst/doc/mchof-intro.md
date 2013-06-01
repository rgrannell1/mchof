<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{Functional Programming with mchof}
-->

Functional Programming with mchof
=================================

## 1 Introduction

## 2 Installation

You have presumably installed mchof if you are reading this vignette; if not
you can simply type
```
install.packages('mchof')
```
Into the console. It is also possible to install the 
development version of mchof by using `install_github('mchof', 'rgrannell1)`, 
though this is not recommended; the development version is usually unstable. 
Updates will be released roughly every month, for the first few releases.

## 3 Functional Programming Basics

## 4 Using `mchof`

### 4.1 Parallel Options

Mchof currently uses mclapply from the parallel library for its parallel backend. 
`mclapply( )` works with very little effort on the part of the user, but unfortunately
it doesn't work on windows (since windows does not allow forking). 

It is likely that this will change in upcoming versions in order to add support for windows;
as of now the most important option is mc.cores, which sets the number of 
processes to spawn. 

For further details see `mclapply( )` in the parallel library for a more complete 
description of the options that can be used with mchof. 

Note that for obvious reasons FUN or X cannot be set in paropts in case they
conflict with f and x (arguments available to most mchof functions).

### 4.2 Multiple Argument Functions

Most functions used with higher-order functions take a fixed number of arguments.
This can be a problem when you want to set optional parameters for the input function.

Take for example, if you want to concatenate the elements of a vector with commas
seperating each element you could use;


```r
comma_paste <- function(vector) {
    paste0(vector, collapse = ", ")
}
Map(comma_paste, list(c("a", "b", "c"), c("1", "2", "3"), c("do", "ray", "me")))
```

```
## [[1]]
## [1] "a, b, c"
## 
## [[2]]
## [1] "1, 2, 3"
## 
## [[3]]
## [1] "do, ray, me"
```


Before handing the multi-parameter paste0 function to `Map( )` it is transformed
into a one-parameter function `comma_paste( )`. This type of transformation is 
known as *currying*. Generally it is more convenient to do this kind of transformation
using an anonymous function.


```r
pasted_vectors <- Map(comma_paste, list(c("a", "b", "c"), c("1", "2", "3"), 
    c("do", "ray", "me")))

mcFilter(function(vector) {
    paste0(vector, collapse = ", ") == "a, b, c"
}, list(c("a", "b", "c"), c("1", "2", "3"), c("do", "ray", "me")))
```

```
## Error: could not find function "mcFilter"
```


### 4.3 NA Handling

This package is composed of many small, composable functions as opposed to larger 
functions with many optional parameters. 

Several functions (including mcFilter, mcReject) take a function that returns
TRUE or FALSE. R uses three-value logic (TRUE, FALSE, NA), so NA must be converted
either TRUE or FALSE internally. Users can control which value
NA is converted to by *composing* their original function with one of these 
three functions;


```r
na_as_true <- function(f) {
    function(...) {
        logical <- as.logical(f(...))
        is.na(logical) || logical
    }
}
```



```r
na_as_false <- function(f) {
    function(...) {
        logical <- as.logical(f(...))
        isTRUE(logical)
    }
}
```



```r
na_as_error <- function(f) {
    function(...) {
        logical <- as.logical(f(...))
        if (is.na(logical)) {
            stop("NA produced", call. = FALSE)
        }
    }
}
```


Each of these functions returns a new, modified function with the appropriate
error handling behaviour.

For example, mcFilter assumes that NA means FALSE. If we were trying to get every
value divisible by two but we also wanted to keep values that produced NA, we 
could use the following


```r
mcFilter(na_as_true(function(x) x%%2), c(1:10, NA))
```

```
## Error: could not find function "mcFilter"
```


### 4.4 Strings as Function Names

Functions like mcFold and mcReduce take binary functions such as plus or multiply.
It would be frustrating to have to write `get('+')` every time you wanted to call
an infix operator, so all mchof functions also accept a function name
in place of an actual function. For example, if you wanted to sum the vector 
1..10 you might write something like; 


```r
mcReduce("+", seq_len(10))
```

```
## Error: could not find function "mcReduce"
```


or


```r
mcFold("+", 0, seq_len(10), list(mc.cores = 2))
```

```
## Error: could not find function "mcFold"
```



```r
options(markdown.HTML.stylesheet = system.file("misc", "vignette.css", package = "knitr"))
```
