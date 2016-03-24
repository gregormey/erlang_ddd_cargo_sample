-module(cargo_read_store).

-record(route_specification,{
	origin = "" :: string(),
	destination = "" :: string()
}).

-record(cargo, {
	id = "" :: string(),
	route_specification = #route_specification{},
	date_created :: tuple()
}).

-export([init/0, 
	load_cargo_by_tracking_id/0, 
	add_cargo/1]).

%% Using ets for the read store is fine for this demo, but the data 
%% will be discarded when the creating process dies, and there is no
%% automatic garbage collection for ets tables.

init() ->
    ets:new(read_store_summary_views, [public, named_table]),
    set_counter_summary(#counter_summary{}),
    ok.

get_counter_summary() -> 
	[{counter_summary, Summary}] = 
		ets:lookup(read_store_summary_views, counter_summary),
	Summary.

set_counter_summary(NewData) -> 
	ets:insert(read_store_summary_views, {counter_summary, NewData}).