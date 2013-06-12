
context("messages: ensure they actually work")

forall(info = "check that error messages work",
	list(
		call_ = c("mcAll(x, f)", "mcAny(x, y)"),
		data_ = list(1, c(1,2), 1:100),
		name_ = list("name", c("a long", "name")),
		name_one_ = list("name", c("a long", "name")),
		name_two_ = list("name", c("a long", "name")),
		func_ = sample(names(messages)),
		which_ = r_seq_len()),
	function (call_, data_ ,name_, name_one_, name_two_, func_, which_) {
		# convert the error message to a string, check
		
		func_ <- messages[[func_]]
		
		text <- tryCatch(
			adapt_call(func_, with = list(
				call = call_, data = data_, 
				name = name_, name_one = name_one_, name_two = name_two_, 
				which = which_)),		
			error = function (err) {
				paste0(err$message)
			},
			warning = function (warn) {
				paste0(warn$message)
			})
		
			grepl(call_, text)
	}
)

