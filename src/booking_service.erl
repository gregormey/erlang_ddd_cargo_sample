%% Copyright (c) 2016, Gregor Meyenberg  <gregor@meyenberg.de>
%% %%
%% %% Permission to use, copy, modify, and/or distribute this software for any
%% %% purpose with or without fee is hereby granted, provided that the above
%% %% copyright notice and this permission notice appear in all copies.
%% %%
%% %% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% %% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% %% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% %% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% %% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% %% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% %% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


-module(booking_service).

-export([book_new_cargo/3, 
		load_cargo_for_routing/1,
		list_all_cargos/0,
		list_shipping_locations/0,
		request_possible_routes_for_cargo/1,
		assign_cargo_to_route/2]).

%% @doc sends command to book a new cargo
-spec book_new_cargo(string(),string(),string())-> ok.
book_new_cargo(Id,Origin,Destination)->
	event_manager:send_command({book_new_cargo, Id ,Origin,Destination}).	

%% @doc loads a cargo by it's tracking ID
-spec load_cargo_for_routing(string())-> atom() | not_found.
load_cargo_for_routing(Tracking_Id)-> 
	cargo_read_store:load_by_id(Tracking_Id) . 
%% @doc loads all cargos in the current state
-spec list_all_cargos() -> list() | no_rows.
list_all_cargos()->
	cargo_read_store:all().

%% @doc lists all possible locations for cargos routings from priv/locations.config
-spec list_shipping_locations() -> list() | no_shipping_locations.
list_shipping_locations() -> 
	case config_helper:get_term_from_config_file("locations.config") of 
		{ok,[Locations]}->Locations;
		{error,_} -> no_shipping_locations
	end.

%% @doc lists all possible routes for a cargo
-spec request_possible_routes_for_cargo(string()) -> list().
request_possible_routes_for_cargo(Tracking_Id) ->
	Cargo=cargo_read_store:load_by_id(Tracking_Id),
	Origin=cargo_read_store:route_specification_origin(Cargo), 
	Destination=cargo_read_store:route_specification_destination(Cargo), 
	graph_traversal_service:find_shortest_path(Origin, Destination). 

%% @doc assigns legs to a existing cargo
-spec assign_cargo_to_route(string(),list())-> ok.
assign_cargo_to_route(Tracking_Id, Legs)->
	event_manager:send_command({assign_cargo_to_route,Tracking_Id,Legs}). 


