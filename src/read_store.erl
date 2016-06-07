
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

-module(read_store).

-export([init/0, 
	load_by_id/2, 
	add/1,
	all/1]).

-record(route_specification,{
	origin = undefined :: undefined | string(),
	destination = undefined :: undefined |string()
}).

-record(cargo, {
	id = undefined :: undefined |string(),
	route_specification = #route_specification{},
	date_created =undefined :: undefined |tuple()
}).

-opaque route_specification() :: #route_specification{}.
-export_type([route_specification/0]).

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
add({cargo,Id,Origin,Destination,DateCreated}) ->
	ok=mnesia_utile:store(#cargo{id=Id,
								route_specification=
									#route_specification{
										origin=Origin,
										destination=Destination
									},
								date_created=DateCreated
								}). 

%% @doc
%% load a cargo from read store by id
-spec load_by_id(atom(),string()) -> tuple() | not_found.
load_by_id(cargo,Tracking_Id) -> 
	mnesia_utile:find_by_id(cargo, Tracking_Id).

%% @doc
%% load all cargos
-spec all(atom()) -> no_rows | list().
all(cargo) -> 
	mnesia_utile:all(cargo).

