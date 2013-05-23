# partition a set into even and odd numbers

mcPartition(
	function (x) x %% 2,
	1:10
) 

# divide a set of combinations into 
# two based on a predicate

mcPartition(
	f = function(pair){
		val <- sum(unlist(pair))
		val > 8
	},
	x = apply(combn(8, 3), 2, list),
	paropts = list(mc.cores = 2)
)
