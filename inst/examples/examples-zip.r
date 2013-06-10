
# zip two vectors into a list of 2-element lists

mcZip(1:5, letters[1:5])

# zip three lists togerther

mcZip(
	list('R', 'Matlab', 'SAS'),
	list('language', 'language', 'languge'),
	list('GNU', 'not GNU', 'not GNU')
)

# zip two lists, with some elements discarded

mcZip(1:10,letters[1:4])

