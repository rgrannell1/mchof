
c("not_a_function", "function_is_required", "formals_has_ellipses", 
"not_a_vector", "vector_is_required", "string_is_required", "not_string", 
"length_mismatch", "not_all_named", "matched_multiple_time", 
"was_factor", "these_were_factors", "not_a_bool", "not_a_number", 
"windows_sequential", "invalid_paropts")

messages$not_a_function("mean(x, y)", 1, "x")

forall(info = "check that error messages work",
	list(call_ = "mcAll(x, f)", "mcAny(x, y)"),
	function (call_) {
		
	}
)
