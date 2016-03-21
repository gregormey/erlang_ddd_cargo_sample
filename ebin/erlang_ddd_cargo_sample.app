{application, erlang_ddd_cargo_sample, [
	{description, "Erlang DDD Cargo Sample "},
	{vsn, "0.0.1"},
	{modules, ['booking_command_handler','cargo_aggregate','erlang_ddd_cargo_sample_app','erlang_ddd_cargo_sample_sup']},
	{registered, [erlang_ddd_cargo_sample_sup]},
	{applications, [kernel,stdlib]},
	{mod, {erlang_ddd_cargo_sample_app, []}}
]}.