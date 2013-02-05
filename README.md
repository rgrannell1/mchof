mchof - MultiCore Higher-Order Functions
===
\* note: this package name is only a placeholder ;)

The benchmarks of an early implementation of mcFilter

```
slowfib <- function(x){
  if(x == 1) 1 else if(x == 2) 2 else slowfib(x-1)+slowfib(x-2)
}

slowfib(2)

rand <- sample(1:30, size = 100, replace = TRUE)

t1 <- system.time(
  Filter(
		f = function(n){
			if(slowfib(n) > 10^4) TRUE else FALSE			
		},
		rand)	
)[3]

t2 <- system.time(
  mcFilter(
		f = function(n){
			if(slowfib(n) > 10^4) TRUE else FALSE			
		},
		rand, list(mc.cores = 2))	
)[3]

t3 <- system.time(
  mcFilter(
  	f = function(n){
			if(slowfib(n) > 10^4) TRUE else FALSE			
		},
		rand, list(mc.cores = 6)))	
)[3]



```


<table>
    <tr>
        <td>Filter</td>
        <td>mcFilter(cores=2)</td>
    </tr>
</table>
