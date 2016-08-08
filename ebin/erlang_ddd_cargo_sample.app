{application, erlang_ddd_cargo_sample, [
	{description, "Erlang DDD Cargo Sample "},
	{vsn, "0.0.1"},
	{modules, ['booking_command_handler','booking_event_handler','booking_service','cargo_aggregate','cargo_projection','cargo_read_store','cargo_repository','cargo_sup','config_helper','erlang_ddd_cargo_sample_app','erlang_ddd_cargo_sample_sup','event_manager','event_store','graph_traversal_service']},
	{registered, [erlang_ddd_cargo_sample_sup]},
	{applications, [kernel,stdlib,mnesia,runtime_tools,wx,observer,uuid,mnesia_utile]},
	{mod, {erlang_ddd_cargo_sample_app, []}}
]}.