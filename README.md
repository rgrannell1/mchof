mchof - MultiCore Higher-Order Functions
===
\* note: this package name is only a placeholder ;)

Map-like higher-order functions such as ```lapply``` are a common part of the R user's vocabulary, and have efficient parallel implementations in packages like ```plyr``` and ```multicore```. This package provides parallel wrappers for other higher-order functions in vanilla R, such as ```Filter``` and ```Position```.

### Performance

A common use case for parallel programming is to try reduce the runtime of problems that are independent of each other and are each fairly computationally heavy in their own right. To simulate this, I took a very slow implementation of the fibonnaci sequence, used it in a predicate function and tested the performance of Filter vs mcFilter.

The benchmarks below were obtained on a low to middle range 64-bit laptop (2 x 2.1 Ghz processor, 4GB Ram).

```
slowfib <- function(x){
    if(x == 1) 1 else if(x == 2) 2 else slowfib(x-1)+slowfib(x-2)
}

pred <- function(n) if(slowfib(n) > 10^4) TRUE else FALSE

rand <- sample(20:30, size = 30, replace = TRUE)

system.time(Filter(pred, rand))
system.time(mcFilter(pred, rand, list(mc.cores = 2)))

```
<table>
    <tr>
        <td>Filter</td>
        <td>mcFilter(cores=2)</td>
    </tr>
        <td> ~102 s </td>
        <td> ~62 s </td>
    <tr>
    
    </tr>
</table>

for computationally intensive processes the performance grows proportional to the number of cores used.
