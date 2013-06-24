
context("mcReforge: normal cases")

register$add(
	"mcReforge", type = "test",
	version = "0.4", is_finished = FALSE
)

forall(info = "mcReforge can set the formals of non-primitive functions",
	list(func_ = r_functions, name_ = r_words()),
	function (func_, name_) {
		
		func_new <- mcReforge( func_, name_ )
		names(formals(func_new)) == name_

	},
	given = function (func_, name_) {
		!is.primitive(func_) && is.function(func_)
	}
)

forall(info = "mcReforge can set the arguments of primitive functions",
	list(func_ = r_functions, name_ = r_words()),
	function (func_, name_) {
		
		func_new <- mcReforge( func_, name_)
		names(arguments(func_new)) == name_

	},
	given = function (func_, name_) {
		is.primitive(func_)
	}
)
