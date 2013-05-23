
# fold over a vector, finding the largest value

mcFold( 
	function (acc, new) {
		if (new > acc) new else acc	
	}, 
	0, sample(1:10))
