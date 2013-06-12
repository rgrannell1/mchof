
all_messages <- names(messages)

forall(info = "check that error messages work",
	list(
		call_ = "mcAll(x, f)", "mcAny(x, y)",
		func_ = all_messages),
	function (call_, func_) {
		
		func_ <- messages[[func_]]
		
	}
)
