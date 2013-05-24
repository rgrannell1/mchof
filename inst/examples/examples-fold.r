
# fold over a vector, finding the largest value

# accumulate a list of employees who have worked for
# over 2 years

mcFold(
	function (acc, new) {
		print(acc)
		if (new$years >= 1) c(acc, new$name)
	}, NULL,
	list(
		list(name = 'Turing', years = 2),
		list(name = 'Leibniz', years = 1),
		list(name = 'Faraday', years = 10),
		list(name = 'Church', years = 4))
)
