
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

-record(route_specification,{
	origin = undefined :: string(),
	destination = undefined :: string()
}).

-record(cargo, {
	id = undefined :: string(),
	route_specification = #route_specification{},
	date_created = undefined:: tuple()
}).

-opaque route_specification() :: #route_specification{}.
-export_type([route_specification/0]).

-opaque cargo() :: #cargo{}.
-export_type([cargo/0]).

-export([init/0, 
	load_cargo_by_tracking_id/1, 
	add_cargo/4,
	all/0]).


init() ->
	case mnesia:create_table(cargo, [{attributes, record_info(fields, cargo)},{disc_copies,[node()]}]) of
    	{atomic,ok} -> ok;
    	{aborted,{already_exists,cargo}} -> already_exists
    end.

add_cargo(Id,DateCreated,Origin,Destination) ->
	mnesia_utile:store(#cargo{
		id=Id, 
		date_created = DateCreated, 
		route_specification=#route_specification{origin=Origin,destination=Destination}
	}). 

load_cargo_by_tracking_id(Tracking_Id) -> 
	mnesia_utile:find_by_id(cargo, Tracking_Id).


all() -> 
	mnesia_utile:all(cargo).

