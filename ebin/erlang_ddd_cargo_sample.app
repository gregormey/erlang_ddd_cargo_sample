{application, erlang_ddd_cargo_sample, [
	{description, "Erlang DDD Cargo Sample "},
	{vsn, "0.0.1"},
	{modules, ['booking_command_handler','cargo_aggregate','cargo_repository','erlang_ddd_cargo_sample_app','erlang_ddd_cargo_sample_sup','event_manager','event_store']},
	{registered, [erlang_ddd_cargo_sample_sup]},
	{applications, [kernel,stdlib,uuid]},
	{mod, {erlang_ddd_cargo_sample_app, []}}
]}.