
# unzip three lists & convert them to two string vectors

mcUnzipWith(
	'paste0',
	list(
		list('Jane', 1),
		list('Jill', 2),
		list('Joan', 3)
))

# get the mean of each 'column' of lists

mcUnzipWith(
	function (x) mean(unlist(x)),
	list(
		list(0.2, 0.10),
		list(0.5, 0.02),
		list(12.2, 0.2)
))
