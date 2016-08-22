
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
%%

-module(cargo_read_store).

-export([init/0, 
	load_by_id/1, 
	route_specification_origin/1,
	route_specification_destination/1,
	add/1,
	assign_to_route/1,
	all/0]).

-record(route_specification,{
	origin = undefined :: undefined | string(),
	destination = undefined :: undefined |string()
}).

-record(itinerary,{
	legs = undefined :: undefined | list()
}).

-record(cargo, {
	id = undefined :: undefined |string(),
	route_specification = #route_specification{},
	itinerary = #itinerary{},
	date_created =undefined :: undefined |tuple()
}).

-opaque route_specification() :: #route_specification{}.
-export_type([route_specification/0]).

-opaque itinerary() :: #itinerary{}.
-export_type([itinerary/0]).

-opaque cargo() :: #cargo{}.
-export_type([cargo/0]).

%% API
%% @doc
%% create tables for the read store
-spec init() -> ok | already_exists.
init() ->
	case mnesia:create_table(cargo, [{attributes, record_info(fields, cargo)},{disc_copies,[node()]}]) of
    	{atomic,ok} -> ok;
    	{aborted,{already_exists,cargo}} -> already_exists
    end.

%% @doc
%% add a cargo projection for read purpose
-spec add(tuple()) -> ok.
add({Id,Origin,Destination,DateCreated}) ->
	ok=mnesia_utile:store(#cargo{id=Id,
								route_specification=
									#route_specification{
										origin=Origin,
										destination=Destination
									},
								date_created=DateCreated
								}). 

-spec assign_to_route(tuple()) -> ok.
assign_to_route({Id,Legs}) ->
	Cargo=load_by_id(Id),
	ok=mnesia_utile:store(Cargo#cargo{id=Id,
								itinerary=#itinerary{legs=Legs}
								}). 


%% @doc
%% load a cargo from read store by id
-spec load_by_id(string()) -> tuple() | not_found.
load_by_id(Tracking_Id) -> 
	mnesia_utile:find_by_id(cargo, Tracking_Id).

%% @doc
%% returns origin of a cargo route specification
-spec route_specification_origin(cargo()) -> string() .
route_specification_origin(#cargo{route_specification=RouteSpecification})->
RouteSpecification#route_specification.origin.


%% @doc
%% returns destination of a cargo route specification
-spec route_specification_destination(cargo()) -> string() .
route_specification_destination(#cargo{route_specification=RouteSpecification})->
RouteSpecification#route_specification.destination.

%% @doc
%% load all cargos
-spec all() -> no_rows | list().
all() -> 
	mnesia_utile:all(cargo).

