{application, erlang_ddd_cargo_sample, [
	{description, "Erlang DDD Cargo Sample "},
	{vsn, "0.0.1"},
	{modules, ['booking_command_handler','booking_event_handler','booking_service','cargo_aggregate','cargo_read_store','cargo_repository','cargo_sup','erlang_ddd_cargo_sample_app','erlang_ddd_cargo_sample_sup','event_manager','event_store']},
	{registered, [erlang_ddd_cargo_sample_sup]},
	{applications, [kernel,stdlib,uuid,mnesia_utile]},
	{mod, {erlang_ddd_cargo_sample_app, []}}
]}.